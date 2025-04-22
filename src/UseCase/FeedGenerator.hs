{-# LANGUAGE OverloadedStrings #-}

module UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeedItem (..),
    OfferFeed (OfferFeed),
    showNewSinceLastVisit,
  )
where

import Data.Text (Text)
import qualified Data.Text.ICU as Locale (LocaleName (Locale))
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Domain.Offer
  ( OfferDetails (_offerDistrict, _offerStreet),
    OfferView
      ( OfferView,
        _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    pricePerMeter,
  )
import UseCase.Offer (QueryAccess (getOffersCreatedAfter))

class FeedPresenter fp where
  present :: fp -> OfferFeed -> IO ()

data OfferFeedItem = OfferFeedItem
  { offerURL :: Text,
    offerTitle :: Text,
    offerDescription :: Text
  }

newtype OfferFeed = OfferFeed [OfferFeedItem]

data Formatters = Formatters
  { cashFormatter :: ICU.NumberFormatter,
    numFormatter :: ICU.NumberFormatter
  }

-- “3‑pok. 65 m² (8 500 zł/m²), 2/5 p., winda, umeblowane,
-- rata ~2 700 zł + 300 zł czynszu – park 200 m, szkoła 100 m, ciche osiedle.”
genTitle :: Formatters -> Domain.Offer.OfferView -> Text
genTitle
  formatters
  ov@Domain.Offer.OfferView
    { _offerLatestPrice = price,
      _offerArea = area,
      _offerTitle = title,
      _offerDetails = details
    } = areaText <> "m\178 | " <> priceText <> ppmText <> locationText <> " | " <> title
    where
      street = details >>= _offerStreet
      district = details >>= _offerDistrict
      ppm = Domain.Offer.pricePerMeter ov
      areaText = formatDouble (numFormatter formatters) area
      priceText = formatIntegral (cashFormatter formatters) price <> "zł"
      ppmText = " | " <> formatDouble (numFormatter formatters) ppm <> "zł/m\178"
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

showNewSinceLastVisit :: (FeedPresenter p, QueryAccess a) => a -> p -> IO ()
showNewSinceLastVisit queryAccess presenter = do
  lastVisit <- lastVisitTime
  formatters <- defaultFormatters
  newOffers <- getOffersCreatedAfter queryAccess lastVisit
  present presenter (OfferFeed $ map (repack formatters) newOffers)
  where
    repack formatters ov@Domain.Offer.OfferView {_offerURL = url} =
      OfferFeedItem
        { offerURL = url,
          offerDescription = description,
          offerTitle = description
        }
      where
        description = genTitle formatters ov

-- Internal

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime = do
  addUTCTime (-(24 * 3600)) <$> getCurrentTime
