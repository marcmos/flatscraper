{-# LANGUAGE FlexibleInstances #-}

module DataAccess.Mobroute where

-- FIXME add explicit export list
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    eitherDecode,
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find)
import Data.Text (Text)
import Data.Time (TimeLocale, hoursToTimeZone, utcToZonedTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import System.Exit (ExitCode (..))
import System.Process.ByteString (readProcessWithExitCode)
import UseCase.GenerateTripSummary hiding (Leg (Leg), Stop (Stop), profileName)
import qualified UseCase.GenerateTripSummary as UseCase
  ( Leg (Leg),
    Stop (Stop),
  )

data MobrouteProfile = MobrouteProfile
  { profileName :: Text,
    profileMaxWalkSeconds :: Int,
    profileFeedIds :: [Int]
  }
  deriving (Show, Eq)

data MobrouteProvider = MobrouteProvider
  { moboExecutablePath :: FilePath,
    moboTimeLocale :: TimeLocale,
    moboProfiles :: [MobrouteProfile]
  }
  deriving (Show)

generateMobrouteInput ::
  MobrouteProvider ->
  (Double, Double) ->
  (Double, Double) ->
  [String] ->
  [String] ->
  UTCTime -> -- Add start time as input
  Text ->
  Maybe BL.ByteString
generateMobrouteInput
  (MobrouteProvider _ timeLocale profiles)
  from
  to
  transferCategories
  outputFormats
  startTime
  profile = do
    (MobrouteProfile _ maxWalkSeconds feedIds) <-
      find (\p -> profileName p == profile) profiles
    return $
      encode $
        object
          [ "feed_ids" .= feedIds,
            "from" .= from,
            "to" .= to,
            "transfer_categories" .= transferCategories,
            "output_formats" .= outputFormats,
            "time" .= formatTime timeLocale "%Y-%m-%dT%H:%M:%S%Ez" localTime, -- Encode time in RFC3339 format
            "max_walk_seconds" .= maxWalkSeconds
          ]
    where
      quirkTimeZoneTime = hoursToTimeZone 2
      localTime = utcToZonedTime quirkTimeZoneTime startTime

type Leg = UseCase.Leg

type Stop = UseCase.Stop

instance ToJSON Leg where
  toJSON leg =
    object
      [ "leg_type" .= legType leg,
        "leg_begin_time" .= legBeginTime leg,
        "leg_end_time" .= legEndTime leg,
        "leg_duration" .= legDuration leg,
        "leg_from_coords" .= legFromCoords leg,
        "leg_to_coords" .= legToCoords leg,
        "walk_to" .= walkTo leg,
        "walk_from" .= walkFrom leg,
        "walk_dist_km" .= walkDistKm leg,
        "trip_route" .= tripRoute leg,
        "trip_route_headsign" .= tripRouteHeadsign leg,
        "trip_from" .= tripFrom leg,
        "trip_to" .= tripTo leg,
        "trip_stops" .= tripStops leg,
        "trip_feed_id" .= tripFeedId leg,
        "trip_agency_id" .= tripAgencyId leg
      ]

instance ToJSON Stop where
  toJSON stop =
    object
      [ "stop_time" .= stopTime stop,
        "stop_name" .= stopName stop,
        "stop_coords" .= stopCoords stop,
        "stop_conn_oid" .= stopConnOid stop
      ]

instance FromJSON Stop where
  parseJSON = withObject "Stop" $ \v ->
    UseCase.Stop
      <$> v .: "stop_time"
      <*> v .: "stop_name"
      <*> v .: "stop_coords"
      <*> v .: "stop_conn_oid"

newtype LegsResponse = LegsResponse
  { legs :: [Leg]
  }
  deriving (Show)

instance FromJSON Leg where
  parseJSON = withObject "Leg" $ \v ->
    UseCase.Leg
      <$> v .: "leg_type"
      <*> v .: "leg_begin_time"
      <*> v .: "leg_end_time"
      <*> v .: "leg_duration"
      <*> v .: "leg_from_coords"
      <*> v .: "leg_to_coords"
      <*> v .:? "walk_to"
      <*> v .:? "walk_from"
      <*> v .:? "walk_dist_km"
      <*> v .:? "trip_route"
      <*> v .:? "trip_route_headsign"
      <*> v .:? "trip_from"
      <*> v .:? "trip_to"
      <*> v .:? "trip_stops"
      <*> v .:? "trip_feed_id"
      <*> v .:? "trip_agency_id"

instance FromJSON LegsResponse where
  parseJSON = withObject "LegsResponse" $ \v ->
    LegsResponse
      <$> v .: "legs"

parseMobrouteResponse :: BL.ByteString -> Either String LegsResponse
parseMobrouteResponse = eitherDecode

callMobroute ::
  MobrouteProvider ->
  (Double, Double) ->
  (Double, Double) ->
  [String] ->
  [String] ->
  UTCTime ->
  Text ->
  Bool ->
  IO (Either String (LegsResponse, BS.ByteString))
callMobroute
  provider
  from
  to
  transferCategories
  outputFormats
  startTime
  profile
  includeStderr = do
    let input =
          generateMobrouteInput
            provider
            from
            to
            transferCategories
            outputFormats
            startTime
            profile
    case input of
      Nothing -> return $ Left "Error generating input for mobroute"
      Just input -> do
        let args = ["route", "-p", BL.unpack input]
        let executablePath = moboExecutablePath provider
        -- print args
        (exitCode, stdout, stderr) <- readProcessWithExitCode executablePath args BS.empty
        case exitCode of
          ExitSuccess ->
            case eitherDecode (BL.fromStrict stdout) of
              Left err -> do
                writeRawOutputToFile executablePath "/tmp/malformed_stdout.json" input
                return $ Left $ "JSON decoding error: " ++ err
              Right decoded ->
                if includeStderr
                  then return $ Right (decoded, stderr)
                  else return $ Right (decoded, BS.empty)
          ExitFailure _ ->
            return . Left $ "Error calling mobroute: Non-zero exit code for input" ++ BL.unpack input

writeRawOutputToFile :: FilePath -> FilePath -> BL.ByteString -> IO ()
writeRawOutputToFile executablePath outputFile input = do
  let args = ["route", "-p", BL.unpack input]
  (_, stdout, _) <- readProcessWithExitCode executablePath args BS.empty
  BL.writeFile outputFile (BL.fromStrict stdout)
  putStrLn $ "Raw output written to " ++ outputFile

getRouteLegs ::
  MobrouteProvider ->
  (Double, Double) ->
  (Double, Double) ->
  UTCTime ->
  Text ->
  Bool ->
  IO (Either String [Leg])
getRouteLegs mobroute startCoords stopCoords startTime profile includeStderr = do
  response <-
    callMobroute
      mobroute
      startCoords
      stopCoords
      ["f", "i", "g"]
      ["legs"]
      startTime
      profile
      includeStderr
  return $ case response of
    Left err -> Left err
    Right (legsResponse, _) -> Right (legs legsResponse)

instance RouteProvider MobrouteProvider where
  getRoute mobroute startCoords stopCoords startTime profile = do
    legs <- getRouteLegs mobroute startCoords stopCoords startTime profile False
    return $ case legs of
      Left err -> Left err
      Right legsList -> Right legsList