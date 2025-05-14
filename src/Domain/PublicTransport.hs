module Domain.PublicTransport where

import Control.Applicative ((<|>)) -- Import <|> for alternative parsing
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM)
import Text.Read (readMaybe)

data Leg = Leg
  { legType :: String,
    legBeginTime :: String,
    legEndTime :: String,
    legDuration :: String,
    legFromCoords :: (Double, Double),
    legToCoords :: (Double, Double),
    walkTo :: Maybe String,
    walkFrom :: Maybe String,
    walkDistKm :: Maybe Double,
    tripRoute :: Maybe String,
    tripRouteHeadsign :: Maybe String,
    tripFrom :: Maybe String,
    tripTo :: Maybe String,
    tripStops :: Maybe [Stop],
    tripFeedId :: Maybe Int,
    tripAgencyId :: Maybe String
  }
  deriving (Show)

data Stop = Stop
  { stopTime :: String,
    stopName :: String,
    stopCoords :: (Double, Double),
    stopConnOid :: Int
  }
  deriving (Show)

data TripSummary = TripSummary
  { totalWalkingTime :: Int, -- Total walking time in seconds
    numberOfTransfers :: Int, -- Number of transfers
    tripStartTime :: UTCTime, -- Trip start time
    tripStartStopName :: String, -- Name of the starting stop
    lineNumbers :: [String], -- List of line numbers used in the trip
    totalTripTime :: Int, -- Total trip time in seconds
    closestHubName :: String -- Name of the closest hub
  }
  deriving (Show)

-- Function to calculate total trip time from all legs
calculateTotalTripTime :: [Leg] -> Either String Int
calculateTotalTripTime legs = do
  durations <- mapM parseDuration legs
  waitingTimes <- calculateWaitingTimes legs
  return $ sum durations + sum waitingTimes
  where
    parseDuration :: Leg -> Either String Int
    parseDuration leg =
      let duration = normalizeDuration (legDuration leg)
       in case words duration of
            [minutes, seconds] ->
              let parseMinutes = readMaybe (stripSuffix 'm' minutes) :: Maybe Int
                  parseSeconds = readMaybe (stripSuffix 's' seconds) :: Maybe Int
               in case (parseMinutes, parseSeconds) of
                    (Just m, Just s) -> Right $ m * 60 + s
                    _ -> Left $ "Invalid duration format: " ++ duration
            [seconds] ->
              let parseSeconds = readMaybe (stripSuffix 's' seconds) :: Maybe Int
               in case parseSeconds of
                    Just s -> Right s
                    _ -> Left $ "Invalid duration format: " ++ duration
            _ -> Left $ "Invalid duration format: " ++ duration

    calculateWaitingTimes :: [Leg] -> Either String [Int]
    calculateWaitingTimes [] = Right []
    calculateWaitingTimes [_] = Right [] -- No waiting time for the last leg
    calculateWaitingTimes (leg1 : leg2 : rest) = do
      endTime <- parseTimeEither ("Invalid end time format: " ++ legEndTime leg1) (legEndTime leg1)
      startTime <- parseTimeEither ("Invalid start time format: " ++ legBeginTime leg2) (legBeginTime leg2)
      let waitingTime = round $ diffUTCTime startTime endTime
      if waitingTime < 0
        then Left $ "Negative waiting time between legs: " ++ show leg1 ++ " and " ++ show leg2
        else (waitingTime :) <$> calculateWaitingTimes (leg2 : rest)

    parseTimeEither :: String -> String -> Either String UTCTime
    parseTimeEither err timeStr =
      case tryParse timeStr of
        Just time -> Right time
        Nothing -> Left err
      where
        tryParse t =
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" t
            <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" t
            <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t

    maybeToEither :: e -> Maybe a -> Either e a
    maybeToEither err Nothing = Left err
    maybeToEither _ (Just x) = Right x

    normalizeDuration :: String -> String
    normalizeDuration [] = []
    normalizeDuration (x : y : xs)
      | x == 'm' && y /= ' ' = 'm' : ' ' : normalizeDuration (y : xs)
      | x == 's' && y /= ' ' = 's' : ' ' : normalizeDuration (y : xs)
      | otherwise = x : normalizeDuration (y : xs)
    normalizeDuration [x] = [x]

    stripSuffix :: Char -> String -> String
    stripSuffix suffix str = if last str == suffix then init str else str

generateTripSummary :: String -> [Leg] -> Either String TripSummary
generateTripSummary closestHubName legs = do
  let (walkingLegs, tripLegs) = partitionLegs legs
  totalWalkingTime <- calculateTotalWalkingTime walkingLegs
  numberOfTransfers <- calculateNumberOfTransfers tripLegs
  tripStartTime <- parseTimeEither "Invalid trip start time" (legBeginTime $ head legs)
  let tripStartStopName = maybe "unknown" id (tripFrom $ head tripLegs)
  let lineNumbers = extractLineNumbers tripLegs
  totalTripTime <- calculateTotalTripTime legs
  return $ TripSummary totalWalkingTime numberOfTransfers tripStartTime tripStartStopName lineNumbers totalTripTime closestHubName
  where
    partitionLegs :: [Leg] -> ([Leg], [Leg])
    partitionLegs = foldr (\leg (walks, trips) -> if legType leg == "walk" then (leg : walks, trips) else (walks, leg : trips)) ([], [])

    calculateTotalWalkingTime :: [Leg] -> Either String Int
    calculateTotalWalkingTime walkingLegs = do
      durations <- mapM parseDuration walkingLegs
      return $ sum durations

    calculateNumberOfTransfers :: [Leg] -> Either String Int
    calculateNumberOfTransfers tripLegs = Right $ max 0 (length tripLegs - 1)

    extractLineNumbers :: [Leg] -> [String]
    extractLineNumbers tripLegs = mapMaybe tripRoute tripLegs

    parseDuration :: Leg -> Either String Int
    parseDuration leg =
      let duration = normalizeDuration (legDuration leg)
       in case words duration of
            [minutes, seconds] ->
              let parseMinutes = readMaybe (stripSuffix 'm' minutes) :: Maybe Int
                  parseSeconds = readMaybe (stripSuffix 's' seconds) :: Maybe Int
               in case (parseMinutes, parseSeconds) of
                    (Just m, Just s) -> Right $ m * 60 + s
                    _ -> Left $ "Invalid duration format: " ++ duration
            [seconds] ->
              let parseSeconds = readMaybe (stripSuffix 's' seconds) :: Maybe Int
               in case parseSeconds of
                    Just s -> Right s
                    _ -> Left $ "Invalid duration format: " ++ duration
            _ -> Left $ "Invalid duration format: " ++ duration

    parseTimeEither :: String -> String -> Either String UTCTime
    parseTimeEither err timeStr =
      case tryParse timeStr of
        Just time -> Right time
        Nothing -> Left err
      where
        tryParse t =
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" t
            <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" t
            <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t

    normalizeDuration :: String -> String
    normalizeDuration [] = []
    normalizeDuration (x : y : xs)
      | x == 'm' && y /= ' ' = 'm' : ' ' : normalizeDuration (y : xs)
      | x == 's' && y /= ' ' = 's' : ' ' : normalizeDuration (y : xs)
      | otherwise = x : normalizeDuration (y : xs)
    normalizeDuration [x] = [x]

    stripSuffix :: Char -> String -> String
    stripSuffix suffix str = if last str == suffix then init str else str