{-# LANGUAGE OverloadedStrings #-}

module Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter)) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.ICU (LocaleName)
import Data.Text.ICU.NumberFormatter (formatDouble, formatIntegral, numberFormatter)
import qualified Data.Text.ICU.NumberFormatter as ICU
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSSChannel (rssItems), RSSItem (rssItemLink), nullChannel, nullItem, nullRSS, rssChannel)
import UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeed (OfferFeed),
    OfferFeedItem
      ( OfferFeedItem,
        offerArea,
        offerDistrict,
        offerPrice,
        offerPricePerMeter,
        offerStreet,
        offerTitle,
        offerURL
      ),
  )

genTitle :: Formatters -> OfferFeedItem -> Text
genTitle
  formatters
  ( OfferFeedItem
      { offerPrice = price,
        offerPricePerMeter = ppm,
        offerArea = area,
        offerTitle = title,
        offerStreet = street,
        offerDistrict = district
      }
    ) = areaText <> "m\178 | " <> priceText <> ppmText <> locationText <> " | " <> title
    where
      areaText = formatDouble (numFormatter formatters) area
      priceText = formatIntegral (cashFormatter formatters) price <> "zł"
      ppmText = " | " <> formatDouble (numFormatter formatters) ppm <> "zł/m\178"
      locationText = case (street, district) of
        (Just s, Just d) -> " | " <> s <> " (" <> d <> ")"
        (Just s, Nothing) -> " | " <> s
        (Nothing, Just d) -> " | " <> d
        _ -> ""

renderOffer :: Formatters -> OfferFeedItem -> RSSItem
renderOffer formatters offer =
  (nullItem title)
    { rssItemLink = Just $ offerURL offer
    }
  where
    title = genTitle formatters offer

renderFeed :: Formatters -> OfferFeed -> Maybe TL.Text
renderFeed formatters (OfferFeed offers) =
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