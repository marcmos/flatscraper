{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UseCase.GenerateTripSummary
  ( generateAndStoreTripSummaries,
    calculateTotalTripTime,
    analyzeTripDurations,
    TripSummary (..),
    TripSummaryDataAccess (..),
    CityTransport (..),
    Leg (..),
    Stop (..),
    RouteProvider (..),
  )
where

import Control.Monad (foldM, forM_)
import Data.Either (rights)
import Data.List (minimumBy)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Domain.PublicTransport
  ( Leg (..),
    Stop (..),
    TripSummary (..),
    calculateTotalTripTime,
    generateTripSummary,
  )
import System.IO (hPutStrLn, stderr)

-- Typeclass for TripSummary data access with functional dependency
class TripSummaryDataAccess da idType | da -> idType where
  fetchRecentCoordinates :: da -> UTCTime -> IO [(idType, Double, Double)]
  storeTripSummary :: da -> idType -> TripSummary -> IO ()

-- Use case to generate and store trip summaries
generateAndStoreTripSummaries ::
  (TripSummaryDataAccess da idType, RouteProvider rp) =>
  da ->
  rp ->
  CityTransport ->
  UTCTime ->
  IO ()
generateAndStoreTripSummaries dataAccess routeProvider cityTransport time = do
  currentTime <- getCurrentTime
  let oneDayAgo = addUTCTime (-(24 * 3600)) currentTime -- 24 hours ago

  -- Fetch recent unprocessed coordinates
  matchedCoords <- fetchRecentCoordinates dataAccess oneDayAgo

  -- Generate and store trip summaries
  forM_ matchedCoords $ \(offerId, lat, lon) -> do
    tripSummaryResult <-
      generateTripSummaryFromRoute
        routeProvider
        cityTransport
        time
        (lat, lon)
    case tripSummaryResult of
      Right tripSummary -> do
        storeTripSummary dataAccess offerId tripSummary
      Left err -> putStrLn $ "Failed to generate trip summary: " ++ err

-- Generate a TripSummary using the RouteProvider
generateTripSummaryFromRoute ::
  (RouteProvider rp) =>
  rp ->
  CityTransport ->
  UTCTime ->
  (Double, Double) ->
  IO (Either String TripSummary)
generateTripSummaryFromRoute routeProvider cityTransport time startCoords = do
  tripResult <- analyzeTripDurations routeProvider cityTransport startCoords time
  return $ case tripResult of
    Nothing -> Left "Failed to analyze trip durations."
    Just (hubName, legs) ->
      generateTripSummary hubName legs

class RouteProvider m where
  getRoute ::
    m ->
    (Double, Double) ->
    (Double, Double) ->
    UTCTime ->
    IO (Either String [Leg])

newtype CityTransport = CityTransport
  { cityHubLocations :: [(String, (Double, Double))]
  }

analyzeTripDurations ::
  (RouteProvider rp) =>
  rp ->
  CityTransport ->
  (Double, Double) ->
  UTCTime ->
  IO (Maybe (String, [Leg]))
analyzeTripDurations routeProvider (CityTransport hubs) startingCoords s = do
  -- Step 1: Analyze trip durations for all hubs
  bestResult <- foldM analyzeHub Nothing hubs
  case bestResult of
    Nothing -> do
      putStrLn "No valid trip durations found for any hub."
      return Nothing
    Just (hubName, bestLegs, bestTime, bestStartTime) -> do
      putStrLn $ "Best hub: " ++ hubName
      putStrLn $ "Best trip time: " ++ show (bestTime `div` 60) ++ " minutes"
      putStrLn $ "Start time for best trip: " ++ show bestStartTime
      putStrLn "Detailed leg information for the best result:"
      mapM_ printLegDetails bestLegs
      return $ Just (hubName, bestLegs)
  where
    analyzeHub ::
      Maybe (String, [Leg], Int, UTCTime) ->
      (String, (Double, Double)) ->
      IO (Maybe (String, [Leg], Int, UTCTime))
    analyzeHub currentBest (hubName, hubCoords) = do
      putStrLn $ "Analyzing trip durations for hub: " ++ hubName
      durations <- mapM (runMoborouteForTime startingCoords hubCoords) [0, 5, 10, 15, 20, 25, 30]
      let validDurations = rights durations
      case validDurations of
        [] -> do
          putStrLn $ "No valid trip durations found for hub: " ++ hubName
          return currentBest
        _ -> do
          let ((bestLegs, bestDuration), bestStartTime) =
                minimumBy (\((_, t1), _) ((_, t2), _) -> compare t1 t2) validDurations
          putStrLn $ "Best trip time for hub " ++ hubName ++ ": " ++ show (bestDuration `div` 60) ++ " minutes"
          putStrLn $ "Start time for best trip: " ++ show bestStartTime
          case currentBest of
            Nothing -> return $ Just (hubName, bestLegs, bestDuration, bestStartTime)
            Just (_, _, currentBestTime, _) ->
              if bestDuration < currentBestTime
                then return $ Just (hubName, bestLegs, bestDuration, bestStartTime)
                else return currentBest

    runMoborouteForTime ::
      (Double, Double) ->
      (Double, Double) ->
      Int ->
      IO (Either String (([Leg], Int), UTCTime))
    runMoborouteForTime startingCoords hubCoords offsetMinutes = do
      let adjustedTime = addUTCTime (fromIntegral (offsetMinutes * 60)) s
      result <- getRoute routeProvider startingCoords hubCoords adjustedTime
      return $ case result of
        Left err -> Left err
        Right legs -> do
          totalTime <- calculateTotalTripTime legs
          Right ((legs, totalTime), adjustedTime)

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
