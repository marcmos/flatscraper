{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UseCase.GenerateTripSummary
  ( generateAndStoreTripSummariesWithComputationTracking,
    calculateTotalTripTime,
    TripSummary (..),
    TripSummaryDataAccess (..),
    CityTransport (..),
    Leg (..),
    Stop (..),
    RouteProvider (..),
  )
where

import Control.Monad (foldM, forM, forM_, unless)
import Data.Either (partitionEithers, rights)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Domain.PublicTransport
  ( Leg (..),
    Stop (..),
    TripSummary (..),
    calculateTotalTripTime,
    generateTripSummary,
  )
import System.IO (hPutStrLn, stderr)

generateAndStoreTripSummariesWithComputationTracking ::
  (Show idType, TripSummaryDataAccess da idType, RouteProvider rp) =>
  da ->
  rp ->
  [Text] ->
  CityTransport ->
  UTCTime ->
  IO ()
generateAndStoreTripSummariesWithComputationTracking dataAccess routeProvider allProfiles cityTransport tripCalcStartTime = do
  currentTime <- getCurrentTime
  let oneDayAgo = addUTCTime (-(24 * 3600)) currentTime

  coordsToProcess <-
    fetchCoordinatesNeedingComputation dataAccess oneDayAgo

  forM_ coordsToProcess $ \(offerId, lat, lon) -> do
    computationAttemptTime <- getCurrentTime

    markComputationAttempt dataAccess offerId "PENDING" computationAttemptTime

    tripSummariesResult <- -- Now an Either String [TripSummary]
      generateTripSummaryFromRoute -- Calls the modified version
        routeProvider
        cityTransport
        tripCalcStartTime
        allProfiles
        (lat, lon)

    case tripSummariesResult of
      Right summaries ->
        if null summaries
          then do
            hPutStrLn stderr $ "No trip summaries could be generated for offer ID " ++ show offerId ++ "."
            markComputationAttempt dataAccess offerId "NO_ROUTE_FOUND" computationAttemptTime
          else do
            hPutStrLn stderr $ "Successfully generated " ++ show (length summaries) ++ " trip summaries for offer ID " ++ show offerId ++ "."
            -- Store each summary and mark success explicitly
            forM_ summaries $ \summary -> do
              storeTripSummary dataAccess offerId summary
            markSuccess dataAccess offerId computationAttemptTime
      -- Note: markSuccess is now handled explicitly after storing each summary.
      Left errMessage -> do
        hPutStrLn stderr $ "Failed to generate trip summaries for offer ID " ++ show offerId ++ ". Error: " ++ errMessage
        let status =
              if any
                (`T.isInfixOf` T.pack errMessage)
                ["No routes found", "Failed to analyze trip durations", "No valid trip durations found", "All hub-specific summary generations failed"]
                then "NO_ROUTE_FOUND"
                else "ERROR"
        markComputationAttempt dataAccess offerId status computationAttemptTime

-- Typeclass for TripSummary data access with functional dependency
class TripSummaryDataAccess da idType | da -> idType where
  fetchCoordinatesNeedingComputation :: da -> UTCTime -> IO [(idType, Double, Double)]
  markComputationAttempt :: da -> idType -> Text -> UTCTime -> IO () -- Args: offerId, status, timestamp
  storeTripSummary :: da -> idType -> TripSummary -> IO () -- Args: offerId, summary
  markSuccess :: da -> idType -> UTCTime -> IO () -- Args: offerId, computationTimestamp

generateTripSummaryFromRoute ::
  (RouteProvider rp) =>
  rp ->
  CityTransport ->
  UTCTime -> -- Base time for route calculations (offsets will be applied to this)
  [Text] -> -- List of all profiles to consider for finding the best route to each hub
  (Double, Double) -> -- Starting coordinates (e.g., of an offer)
  IO (Either String [TripSummary]) -- One TripSummary per hub, representing the best route found
generateTripSummaryFromRoute
  routeProvider
  (CityTransport hubs) -- List of (hubName, hubCoords)
  baseSearchTime
  allProfilesToConsider
  offerStartCoords = do
    -- For each hub, find the best trip (legs, duration, start time, and profile used)
    -- by considering all specified profiles and various time offsets.
    bestOverallTripsToHubs <- fmap catMaybes $ forM hubs $ uncurry findBestTripToHubAcrossProfiles

    if null bestOverallTripsToHubs
      then do
        hPutStrLn stderr $ "No routes found to any hub for offer at " ++ show offerStartCoords ++ " using any profile."
        return $ Left "No routes found to any hub using any profile."
      else do
        hPutStrLn stderr $ "Found best routes to " ++ show (length bestOverallTripsToHubs) ++ " hubs from " ++ show offerStartCoords
        -- Convert these best trips into TripSummary objects
        let tripSummaryEithers =
              map
                ( \(foundProfileName, foundHubName, foundLegs) ->
                    Domain.PublicTransport.generateTripSummary foundHubName (T.unpack foundProfileName) foundLegs
                )
                bestOverallTripsToHubs

        let (failures, successes) = partitionEithers tripSummaryEithers

        unless (null failures) $ forM_ (zip bestOverallTripsToHubs tripSummaryEithers) $ \(((prof, hubN, _)), result) ->
          case result of
            Left err -> hPutStrLn stderr $ "Failed to generate TripSummary for hub " ++ hubN ++ " (using profile " ++ T.unpack prof ++ "): " ++ err
            Right _ -> return ()

        if null successes
          then return $ Left "All attempts to generate TripSummaries from found routes failed."
          else return $ Right successes
    where
      -- Helper: Finds the best trip to a specific hub by checking all profiles and time offsets.
      -- Returns Maybe (profileNameForBestTrip, hubName, legsForBestTrip)
      findBestTripToHubAcrossProfiles :: String -> (Double, Double) -> IO (Maybe (Text, String, [Leg]))
      findBestTripToHubAcrossProfiles targetHubName targetHubCoords = do
        hPutStrLn stderr $ "Analyzing routes to hub: " ++ targetHubName

        -- Gather all potential successful trips to this hub from all profiles and offsets
        -- Each element: (profileName, legs, duration, startTime)
        allPotentialTripsForThisHub <- fmap catMaybes $ forM allProfilesToConsider $ \currentProfileName -> do
          putStrLn $ "  Checking profile: " ++ T.unpack currentProfileName ++ " for hub " ++ targetHubName

          resultsForProfileAndHub <- mapM (runMoborouteForSingleRouteQuery currentProfileName offerStartCoords targetHubCoords) [0, 5, 10, 15, 20, 25, 30]
          let validLegsAndDurationsWithTimes = rights resultsForProfileAndHub -- Each element is ((legs, duration), startTime)
          case validLegsAndDurationsWithTimes of
            [] -> do
              -- putStrLn $ "    No valid routes found for profile " ++ T.unpack currentProfileName ++ " to hub " ++ targetHubName
              return Nothing
            _ -> do
              let ((bestLegsForThisProfile, bestDurationForThisProfile), bestStartTimeForThisProfile) =
                    minimumBy (\((_, d1), _) ((_, d2), _) -> compare d1 d2) validLegsAndDurationsWithTimes
              -- putStrLn $ "    Best trip for profile " ++ T.unpack currentProfileName ++ " to hub " ++ targetHubName ++
              --            ": duration " ++ show (bestDurationForThisProfile `div` 60) ++ " mins, start " ++ show bestStartTimeForThisProfile
              return $ Just (currentProfileName, bestLegsForThisProfile, bestDurationForThisProfile, bestStartTimeForThisProfile)

        case allPotentialTripsForThisHub of
          [] -> do
            hPutStrLn stderr $ "  No successful routes found to hub " ++ targetHubName ++ " across any profile."
            return Nothing
          trips@(_ : _) -> do
            let (overallBestProfile, overallBestLegs, overallBestDuration, overallBestStartTime) =
                  minimumBy (\(_, _, d1, _) (_, _, d2, _) -> compare d1 d2) trips

            hPutStrLn stderr $
              "  Overall best trip to hub "
                ++ targetHubName
                ++ " is via profile "
                ++ T.unpack overallBestProfile
                ++ ", duration "
                ++ show (overallBestDuration `div` 60)
                ++ " mins, start "
                ++ show overallBestStartTime
                ++ "."

            -- Uncomment to print detailed leg information for the selected best trip
            -- putStrLn $ "  Detailed leg information for best trip to hub " ++ targetHubName ++ ":"
            -- mapM_ printLegDetails overallBestLegs

            return $ Just (overallBestProfile, targetHubName, overallBestLegs)

      -- Helper: Executes a single route query for a given profile, start/end coords, and time offset.
      runMoborouteForSingleRouteQuery :: Text -> (Double, Double) -> (Double, Double) -> Int -> IO (Either String (([Leg], Int), UTCTime))
      runMoborouteForSingleRouteQuery profileNameToUse sCoords tCoords offsetMinutes = do
        let adjustedTime = addUTCTime (fromIntegral (offsetMinutes * 60)) baseSearchTime
        routeResult <-
          getRoute
            routeProvider
            sCoords
            tCoords
            adjustedTime
            profileNameToUse
        return $ case routeResult of
          Left err -> Left err
          Right legs ->
            case Domain.PublicTransport.calculateTotalTripTime legs of
              Left errCalc -> Left ("Failed to calculate total trip time. Error: " ++ errCalc)
              Right totalTime -> Right ((legs, totalTime), adjustedTime)

class RouteProvider m where
  getRoute ::
    m ->
    (Double, Double) ->
    (Double, Double) ->
    UTCTime ->
    Text ->
    IO (Either String [Leg])

newtype CityTransport = CityTransport
  { cityHubLocations :: [(String, (Double, Double))]
  }

printLegDetails :: Leg -> IO ()
printLegDetails leg = do
  putStrLn $ "Leg Type: " ++ legType leg
  putStrLn $ "  Begin Time: " ++ show (legBeginTime leg)
  putStrLn $ "  End Time: " ++ show (legEndTime leg)
  putStrLn $ "  Duration: " ++ legDuration leg
  putStrLn $ "  From Coordinates: " ++ show (legFromCoords leg)
  putStrLn $ "  To Coordinates: " ++ show (legToCoords leg)
  case legType leg of
    "walk" -> do
      putStrLn $ "  Walk From: " ++ maybe "unknown" id (walkFrom leg)
      putStrLn $ "  Walk To: " ++ maybe "unknown" id (walkTo leg)
      putStrLn $ "  Walk Distance (km): " ++ maybe "unknown" show (walkDistKm leg)
    "trip" -> do
      putStrLn $ "  Trip Route: " ++ maybe "unknown" id (tripRoute leg)
      putStrLn $ "  Trip Route Headsign: " ++ maybe "unknown" id (tripRouteHeadsign leg)
      putStrLn $ "  Trip From: " ++ maybe "unknown" id (tripFrom leg)
      putStrLn $ "  Trip To: " ++ maybe "unknown" id (tripTo leg)
      putStrLn $ "  Trip Stops: " ++ maybe "unknown" show (tripStops leg)
      putStrLn $ "  Trip Feed ID: " ++ maybe "unknown" show (tripFeedId leg)
      putStrLn $ "  Trip Agency ID: " ++ maybe "unknown" show (tripAgencyId leg)
    _ -> putStrLn "  Unknown leg type"
