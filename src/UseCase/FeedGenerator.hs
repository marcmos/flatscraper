{-# LANGUAGE OverloadedStrings #-}

module UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeedItem (..),
    OfferFeed (OfferFeed),
    showNewSinceLastVisit,
  )
where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.ICU as Locale (LocaleName (Locale))
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
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
import Text.Blaze.Html5 (Html)
import UseCase.Offer (QueryAccess (getOffersCreatedAfter))

class FeedPresenter fp where
  present :: fp -> OfferFeed -> IO ()

data OfferFeedItem = OfferFeedItem
  { offerURL :: Text,
    offerTitle :: Text,
    offerDescription :: Text,
    offerHasElevator :: Maybe HasElevator,
    offerIsAccessible :: Maybe Bool,
    offerFloorText :: Maybe Text,
    offerAreaText :: Maybe Text,
    offerPriceText :: Maybe Text,
    offerPricePerAreaText :: Maybe Text,
    offerLocationText :: Maybe Text,
    offerBuildYearText :: Maybe Text
  }

newtype OfferFeed = OfferFeed [OfferFeedItem]

data Formatters = Formatters
  { cashFormatter :: ICU.NumberFormatter,
    numFormatter :: ICU.NumberFormatter
  }

priceText :: Formatters -> OfferView -> Text
priceText (Formatters {numFormatter = formatter}) (OfferView {_offerLatestPrice = price}) =
  formatIntegral formatter price <> "zł"

areaText :: Formatters -> OfferView -> Text
areaText (Formatters {numFormatter = formatter}) (OfferView {_offerArea = area}) =
  formatDouble formatter area <> "m\178"

ppmText :: Formatters -> OfferView -> Text
ppmText (Formatters {numFormatter = formatter}) ov =
  -- Using numFormatter instead of cashFormatter is no mistake here.
  -- CashFormatter shows in cent-value precision, which is too much.
  formatDouble formatter (pricePerMeter ov) <> "zł/m\178"

-- “3‑pok. 65 m² (8 500 zł/m²), 2/5 p., winda, umeblowane,
-- rata ~2 700 zł + 300 zł czynszu – park 200 m, szkoła 100 m, ciche osiedle.”
genTitle :: Formatters -> OfferView -> Text
genTitle
  formatters
  ov@OfferView
    { _offerTitle = title,
      _offerDetails = details
    } =
    areaText formatters ov
      <> " | "
      <> priceText formatters ov
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

showNewSinceLastVisit :: (FeedPresenter p, QueryAccess a) => a -> p -> IO ()
showNewSinceLastVisit queryAccess presenter = do
  lastVisit <- lastVisitTime
  formatters <- defaultFormatters
  newOffers <- getOffersCreatedAfter queryAccess lastVisit
  present presenter (OfferFeed $ map (repack formatters) newOffers)
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
       in OfferFeedItem
            { offerURL = url,
              offerDescription = description,
              offerTitle = _offerTitle ov,
              offerHasElevator = elevator,
              offerIsAccessible =
                if ((\(HasElevator {_hasElevator = e}) -> e) <$> elevator) == Just True
                  then Just True
                  else do
                    f <- _offerDetails ov >>= _offerPropertyFloor
                    if f <= 2 then Just True else Nothing,
              offerFloorText = floorText,
              offerPriceText = Just $ priceText formatters ov,
              offerAreaText = Just $ areaText formatters ov,
              offerPricePerAreaText = Just $ ppmText formatters ov,
              offerLocationText = _offerDetails ov >>= _offerStreet,
              offerBuildYearText = (\yr -> "rok " <> toText yr) <$> (_offerDetails ov >>= _offerBuiltYear)
            }
      where
        description = genTitle formatters ov

-- Internal

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime = do
  addUTCTime (-(24 * 3600)) <$> getCurrentTime
