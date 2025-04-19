{-# LANGUAGE OverloadedStrings #-}

module Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter)) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Text.ICU (LocaleName)
import qualified Data.Text.ICU.Locale as Locale
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSSChannel (rssItems), RSSItem (rssItemLink), nullChannel, nullItem, nullRSS, rssChannel)
import UseCase.FeedGenerator (FeedPresenter (present))
import UseCase.Offer
  ( OfferDetails (OfferDetails, _offerDistrict, _offerStreet),
    OfferView
      ( OfferView,
        _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    offerStreet,
  )

pricePerMeter :: OfferView -> Double
pricePerMeter offer = fromIntegral (_offerLatestPrice offer) / _offerArea offer

genTitle :: Formatters -> OfferView -> Text
genTitle
  formatters
  offer@( OfferView
            { _offerLatestPrice = price,
              _offerArea = area,
              _offerDetails = offerDetails,
              _offerTitle = title
            }
          ) = areaText <> "m\178 | " <> priceText <> ppm <> locationText offerDetails <> " | " <> title
    where
      areaText = formatDouble (numFormatter formatters) area
      priceText = formatIntegral (cashFormatter formatters) price <> "zł"
      ppm = " | " <> formatDouble (numFormatter formatters) (pricePerMeter offer) <> "zł/m\178"
      locationText
        ( Just
            ( OfferDetails
                { _offerStreet = Just street,
                  _offerDistrict = Just district
                }
              )
          ) =
          " | " <> street <> " (" <> district <> ")"
      locationText (Just OfferDetails {_offerDistrict = Just district}) =
        " | (" <> district <> ")"
      locationText (Just (OfferDetails {_offerStreet = Just street})) =
        " | " <> street
      locationText _ = ""

renderOffer :: Formatters -> OfferView -> RSSItem
renderOffer formatters offer =
  (nullItem title)
    { rssItemLink = Just $ _offerURL offer
    }
  where
    title = genTitle formatters offer

renderFeed :: Formatters -> [OfferView] -> Maybe TL.Text
renderFeed formatters offers =
  textRSS $
    (nullRSS "" "")
      { rssChannel = (nullChannel "flatscraper" "") {rssItems = renderOffer formatters <$> offers}
      }

data Formatters = Formatters
  { cashFormatter :: ICU.NumberFormatter,
    numFormatter :: ICU.NumberFormatter
  }

newtype RSSFeedPresenter = RSSFeedPresenter LocaleName

instance FeedPresenter RSSFeedPresenter where
  present (RSSFeedPresenter locale) offers = do
    cashFormatter <- numberFormatter "precision-currency-cash" locale
    pricePerMeterFormatter <- numberFormatter "precision-integer" locale
    let formatters = Formatters cashFormatter pricePerMeterFormatter
    let feed =
          fromMaybe
            "Failed to generate feed"
            ( renderFeed
                formatters
                offers
            )
    TL.putStrLn feed