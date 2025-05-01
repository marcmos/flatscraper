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
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.ICU as Locale (LocaleName (Locale))
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import Data.Time (LocalTime (..), UTCTime, getCurrentTime, localDay, localTimeToUTC, midnight, utcToLocalTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Domain.Offer
  ( HasElevator (HasElevator, _hasElevator),
    OfferDetails
      ( _offerBuildingFloors,
        _offerBuiltYear,
        _offerDistrict,
        _offerMarket,
        _offerMunicipalityArea,
        _offerPropertyFloor,
        _offerRooms,
        _offerStreet
      ),
    OfferMarket (MarketPrimary, MarketSecondary),
    OfferView
      ( OfferView,
        _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    hasElevator,
    pricePerMeter,
  )
import UseCase.Offer (QueryAccess (getOffersCreatedAfter))

class FeedPresenter fp a where
  present :: fp a -> OfferFeed -> IO a

data OfferFeedItem = OfferFeedItem
  { offerURL :: Text,
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
    offerRooms :: Maybe Int
  }

data Formatters = Formatters
  { cashFormatter :: ICU.NumberFormatter,
    numFormatter :: ICU.NumberFormatter
  }

data OfferFeed = OfferFeed Formatters [OfferFeedItem]

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

showNewSinceLastVisit ::
  (FeedViewer fv, FeedPresenter fp a, QueryAccess qa) =>
  qa ->
  fp a ->
  fv a ->
  Maybe (OfferView -> Bool) ->
  IO ()
showNewSinceLastVisit queryAccess presenter viewer offerFilter = do
  lastVisit <- lastVisitTime
  formatters <- defaultFormatters
  allOffers <- getOffersCreatedAfter queryAccess lastVisit
  let newOffers = maybe allOffers (`filter` allOffers) offerFilter
  let offers = map (repack formatters) newOffers
  let sortedOffers =
        sortOn
          (\(OfferFeedItem {offerPricePerMeter = ppm}) -> ppm)
          offers
  feedText <- present presenter (OfferFeed formatters sortedOffers)
  let title =
        "Specjalnie dla Ciebie przygotowałem "
          <> (toText . length $ newOffers)
          <> " ofert mieszkań do przeglądnięcia"
  view viewer title feedText
  where
    repack formatters ov@OfferView {_offerURL = url} =
      let pFloor = _offerDetails ov >>= _offerPropertyFloor
          bFloors = _offerDetails ov >>= _offerBuildingFloors
          elevator = hasElevator ov
          floorText = case (pFloor, bFloors) of
            (Just 0, Just b) -> Just $ "parter/" <> toText b
            (Just 0, Nothing) -> Just "parter"
            (Just p, Just b) -> Just $ "piętro " <> toText p <> "/" <> toText b
            (Just p, _) -> Just $ "piętro " <> toText p
            _ -> Nothing
          isAccessibleFloor = (>=) 2 <$> pFloor
       in OfferFeedItem
            { offerURL = url,
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
              offerRooms = _offerDetails ov >>= _offerRooms
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
  addUTCTime (1 * 3600) <$> getTodayMidnightUTC