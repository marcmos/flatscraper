{-# LANGUAGE MultiParamTypeClasses #-}

module UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeedItem (..),
    OfferFeed (OfferFeed),
    showNewSinceLastVisit,
    Formatters (Formatters),
    priceText,
    areaText',
    ppmText',
    toText,
    FeedViewer (view),
    LastVisitStorer (storeLastVisit, getLastVisit),
    OfferMarket (MarketPrimary, MarketSecondary),
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (second))
import Data.List (find, nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isInfixOf, isPrefixOf, pack)
import qualified Data.Text.ICU as Locale (LocaleName (Locale))
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import Data.Text.IO (hPutStrLn)
import Data.Time (LocalTime (..), UTCTime, getCurrentTime, localDay, localTimeToUTC, midnight, utcToLocalTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Domain.Offer
  ( HasElevator (HasElevator, _hasElevator),
    OfferDetails (_offerBuildingFloors, _offerBuiltYear, _offerDistrict, _offerHasElevator, _offerMarket, _offerMunicipalityArea, _offerPropertyFloor, _offerRooms, _offerStreet),
    OfferMarket (MarketPrimary, MarketSecondary),
    OfferView
      ( OfferView,
        _offerArea,
        _offerCreatedAt,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    hasElevator,
    pricePerMeter,
  )
import Domain.PublicTransport
  ( TripSummary,
    profileName,
    totalTripTime,
    totalWalkingTime,
  )
import System.IO (stderr)
import UseCase.Offer (QueryAccess (fetchTripSummaries, getOffersCreatedAfter))

class FeedPresenter fp a where
  present :: fp a -> OfferFeed -> IO a

data OfferFeedItem = OfferFeedItem
  { offerURL :: [Text],
    offerTitle :: Text,
    offerDescription :: Text,
    offerHasElevator :: Maybe HasElevator,
    offerIsAccessible :: Maybe Bool,
    offerFloorText :: Maybe Text,
    offerArea :: Double,
    offerPrice :: Int,
    offerPricePerMeter :: Double,
    offerStreetText :: Maybe Text,
    offerDistrictText :: Maybe Text,
    offerBuildYearText :: Maybe Text,
    offerRooms :: Maybe Int,
    offerMunicipalityArea :: Maybe Text,
    offerTripSummaries :: [TripSummary],
    offerMarket :: Maybe OfferMarket,
    offerPublishTime :: Maybe UTCTime
  }

data Formatters = Formatters
  { cashFormatter :: ICU.NumberFormatter,
    numFormatter :: ICU.NumberFormatter
  }

data OfferFeed = OfferFeed Formatters [(Text, [OfferFeedItem])]

priceText :: Formatters -> Int -> Text
priceText (Formatters {numFormatter = formatter}) price =
  formatIntegral formatter price <> "zł"

areaText' :: Formatters -> Double -> Text
areaText' (Formatters {numFormatter = formatter}) area =
  formatDouble formatter area <> "m\178"

ppmText' :: Formatters -> Double -> Text
ppmText' (Formatters {numFormatter = formatter}) ppm =
  -- Using numFormatter instead of cashFormatter is no mistake here.
  -- CashFormatter shows in cent-value precision, which is too much.
  formatDouble formatter ppm <> "zł/m\178"

ppmText :: Formatters -> OfferView -> Text
ppmText formatters ov =
  ppmText' formatters (pricePerMeter ov)

-- “3‑pok. 65 m² (8 500 zł/m²), 2/5 p., winda, umeblowane,
-- rata ~2 700 zł + 300 zł czynszu – park 200 m, szkoła 100 m, ciche osiedle.”
genTitle :: Formatters -> OfferView -> Text
genTitle
  formatters
  ov@OfferView
    { _offerTitle = title,
      _offerDetails = details,
      _offerArea = area,
      _offerLatestPrice = price
    } =
    areaText' formatters area
      <> " | "
      <> priceText formatters price
      <> " | "
      <> ppmText formatters ov
      <> " | "
      <> locationText
      <> " | "
      <> title
    where
      street = details >>= _offerStreet
      district = details >>= _offerDistrict
      locationText = case (street, district) of
        (Just s, Just d) -> " | " <> s <> " (" <> d <> ")"
        (Just s, Nothing) -> " | " <> s
        (Nothing, Just d) -> " | " <> d
        _ -> ""

defaultFormatters :: IO Formatters
defaultFormatters = do
  let locale = Locale.Locale "pl_PL"
  moneyFormatter <- numberFormatter "precision-currency-cash" locale
  pricePerMeterFormatter <- numberFormatter "precision-integer" locale
  return $ Formatters moneyFormatter pricePerMeterFormatter

toText :: (Show a) => a -> Text
toText = T.pack . show

class FeedViewer fv where
  view :: fv a -> Text -> a -> IO ()

class LastVisitStorer vs u where
  storeLastVisit :: vs -> u -> UTCTime -> IO ()
  getLastVisit :: vs -> u -> IO (Maybe UTCTime)

deduplicateOffers :: [OfferFeedItem] -> [OfferFeedItem]
deduplicateOffers = foldr mergeItems []
  where
    mergeItems item acc =
      case break (\x -> hasSameArea item x && hasSamePrice item x) acc of
        (before, matching : after) ->
          let merged =
                matching
                  { offerURL = offerURL item ++ offerURL matching,
                    offerStreetText = pickFirstNonEmpty offerStreetText item matching,
                    offerDistrictText = pickFirstNonEmpty offerDistrictText item matching,
                    offerMunicipalityArea = pickFirstNonEmpty offerMunicipalityArea item matching,
                    offerTripSummaries =
                      nub $
                        offerTripSummaries item ++ offerTripSummaries matching
                  }
           in before ++ (merged : after)
        _ -> item : acc
    hasSameArea item1 item2 = offerArea item1 == offerArea item2
    hasSamePrice item1 item2 = offerPrice item1 == offerPrice item2

    -- Helper function to prioritize values based on source order
    pickFirstNonEmpty :: (OfferFeedItem -> Maybe a) -> OfferFeedItem -> OfferFeedItem -> Maybe a
    pickFirstNonEmpty field item1 item2 =
      case (field item1, field item2) of
        (Just val1, _) | isFromPreferredSource item1 -> Just val1
        (_, Just val2) | isFromPreferredSource item2 -> Just val2
        (val1, val2) -> val1 <|> val2

    -- Helper function to determine if an item is from a preferred source
    isFromPreferredSource :: OfferFeedItem -> Bool
    isFromPreferredSource item
      | any ("otodom" `T.isInfixOf`) (offerURL item) = True
      | any ("nieruchomosci-online" `T.isInfixOf`) (offerURL item) = True
      | any ("morizon" `T.isInfixOf`) (offerURL item) = False
      | otherwise = False

selectInterestingTrips :: [TripSummary] -> [TripSummary]
selectInterestingTrips summaries =
  if null summaries
    then []
    else
      let -- Attempt to find a tram trip
          tramTripM =
            find
              ( \ts ->
                  "tram_"
                    `T.isPrefixOf` (T.pack $ profileName ts)
              )
              summaries

          -- Attempt to find the trip with the minimum total time
          minTotalTimeTripM = listToMaybe $ sortOn totalTripTime summaries

          -- Initial selection of candidates
          initialCandidates = catMaybes [tramTripM, minTotalTimeTripM]

          longWalkThreshold = 6 * 60

          -- Conditionally add the trip with the minimum walking time
          finalCandidates = case minTotalTimeTripM of
            Just minTotalTrip
              | totalWalkingTime minTotalTrip > longWalkThreshold ->
                  -- If shortest total time trip's walk time is > 6min, find
                  -- the overall shortest walking time trip
                  let minWalkingTimeTripM =
                        listToMaybe $
                          sortOn totalWalkingTime $
                            -- ...but do not propose walk, that isn't better
                            -- than the first one
                            filter
                              (\ts -> totalWalkingTime ts <= longWalkThreshold)
                              summaries
                   in initialCandidates ++ catMaybes [minWalkingTimeTripM]
            -- Otherwise, stick with the initial candidates
            _ -> initialCandidates
       in -- Remove duplicates from the final selection
          nub finalCandidates

showNewSinceLastVisit ::
  (FeedViewer fv, FeedPresenter fp a, QueryAccess qa) =>
  qa ->
  fp a ->
  fv a ->
  IO (Maybe UTCTime) ->
  ([OfferView] -> [(Text, [OfferView])]) ->
  ([(Text, [OfferView])] -> Text) ->
  IO ()
showNewSinceLastVisit queryAccess presenter viewer fetchLastVisit offerGroupper title = do
  visitTimeFallback <- lastVisitTime
  lastVisit <- fetchLastVisit

  formatters <- defaultFormatters

  allOffers <- getOffersCreatedAfter queryAccess (fromMaybe visitTimeFallback lastVisit)

  let grouppedOffers = offerGroupper allOffers

  if null grouppedOffers
    then hPutStrLn stderr "No offers to show"
    else do
      offers <-
        mapM
          ( \(groupTitle, offerViews) -> do
              initialOfferFeedItems <- mapM (repack formatters queryAccess) offerViews
              let deduplicatedGroupItems = deduplicateOffers initialOfferFeedItems
              -- Apply selectInterestingTrips to *all* items in the deduplicated list
              -- to ensure non-merged items also have their summaries refined.
              let finalOfferFeedItems =
                    map
                      ( \item ->
                          item
                            { offerTripSummaries =
                                selectInterestingTrips (offerTripSummaries item)
                            }
                      )
                      deduplicatedGroupItems
              return (groupTitle, finalOfferFeedItems)
          )
          grouppedOffers

      let deduplicatedOffers = map (second deduplicateOffers) offers
      feedText <-
        present
          presenter
          ( OfferFeed
              formatters
              deduplicatedOffers
          )

      view viewer (title grouppedOffers) feedText
      hPutStrLn stderr $ "Displayed " <> toText (length allOffers) <> " offers"
  where
    repack formatters queryAccess ov@OfferView {_offerURL = url} = do
      tripSummaries <- fetchTripSummaries queryAccess url -- Fetch TripSummary
      let pFloor = _offerDetails ov >>= _offerPropertyFloor
          bFloors = _offerDetails ov >>= _offerBuildingFloors
          elevator = hasElevator ov
          municipalityArea = _offerDetails ov >>= _offerMunicipalityArea
          floorText = case (pFloor, bFloors) of
            (Just 0, Just b) -> Just $ "parter/" <> toText b
            (Just 0, Nothing) -> Just "parter"
            (Just p, Just b) -> Just $ "piętro " <> toText p <> "/" <> toText b
            (Just p, _) -> Just $ "piętro " <> toText p
            _ -> Nothing
          isAccessibleFloor = (>=) 2 <$> pFloor
      return $
        OfferFeedItem
          { offerURL = [url],
            offerDescription = description,
            offerTitle = _offerTitle ov,
            offerHasElevator = elevator,
            offerIsAccessible =
              case elevator of
                Just (HasElevator True Nothing) -> Just True
                Just (HasElevator False _)
                  | isAccessibleFloor == Just True -> Just True
                  | isAccessibleFloor == Just False -> Just False
                Just (HasElevator True (Just _)) -> Just True
                _ -> do
                  f <- pFloor
                  case f of
                    x | x <= 2 -> Just True
                    _ -> do
                      bYear <- _offerDetails ov >>= _offerBuiltYear
                      if bYear <= 1970 then Just False else Nothing,
            offerFloorText = floorText,
            offerPrice = _offerLatestPrice ov,
            offerArea = _offerArea ov,
            offerPricePerMeter = pricePerMeter ov,
            offerStreetText = _offerDetails ov >>= _offerStreet,
            offerDistrictText = _offerDetails ov >>= _offerDistrict,
            offerBuildYearText =
              (\yr -> "rok " <> toText yr)
                <$> (_offerDetails ov >>= _offerBuiltYear),
            offerRooms = _offerDetails ov >>= _offerRooms,
            offerMunicipalityArea = municipalityArea,
            offerTripSummaries = tripSummaries, -- Populate TripSummary
            offerMarket = _offerDetails ov >>= _offerMarket,
            offerPublishTime = _offerCreatedAt ov
          }
      where
        description = genTitle formatters ov

-- Internal

getTodayMidnightUTC :: IO UTCTime
getTodayMidnightUTC = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localTime = utcToLocalTime timezone now
      day = localDay localTime
      midnightLocal = LocalTime day midnight
      midnightUTC = localTimeToUTC timezone midnightLocal
  return midnightUTC

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime =
  -- addUTCTime (-(7 * 3600)) <$> getTodayMidnightUTC
  -- addUTCTime (-(24 * 3600)) <$> getCurrentTime
  addUTCTime (-(10 * 3600)) <$> getTodayMidnightUTC