{-# LANGUAGE OverloadedStrings #-}

module Scraper.OtodomScraper
  ( scraper,
  )
where

import Control.Lens
  ( non,
    over,
    (&),
    (.~),
    (?~),
    (^?),
    (^?!),
  )
import Control.Monad ()
import Data.Aeson (Value (), decodeStrict)
import Data.Aeson.Lens (AsNumber (_Integer), key, _String)
import Data.Either.Combinators (rightToMaybe)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Text.Lens ()
import qualified Data.Text.Read as T (double)
import Persistence.ScrapeLoader (ScraperPack (ScraperPack), WebScraper, prefixWebScraper)
import Scraper.Common (parsePrice)
import Text.HTML.Scalpel (Scraper, attr, chroots, text, texts, (//), (@:), (@=))
import UseCase.Offer
  ( OfferView
      ( OfferView,
        _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    emptyDetails,
    newOfferView,
    offerArea,
    offerDescription,
    offerDetails,
    offerDistrict,
    offerRooms,
    offerStreet,
    offerTitle,
  )

offerScraper :: Scraper Text OfferView
offerScraper = do
  name <- text ("p" @: ["data-cy" @= "listing-item-title"])
  price <- parsePrice . fromMaybe "0 zł" . find (T.isSuffixOf "zł") <$> texts "span"
  url <- ("https://www.otodom.pl" <>) <$> attr "href" "a"
  return $ newOfferView url price 0.0 name

parseNum :: Maybe Text -> Maybe Double
parseNum x = do
  a <- x
  b <- rightToMaybe $ T.double a
  return $ fst b

fromJSON :: Text -> Maybe OfferView -> Maybe OfferView
fromJSON input offer =
  let bs = T.encodeUtf8 input
      json = decodeStrict bs :: Maybe Value
      ad = (^?! key "props" . key "pageProps" . key "ad") <$> json
      price = ad >>= (^? key "target" . key "Price" . _Integer)
      street = ad >>= (^? key "location" . key "address" . key "street" . key "name" . _String)
      district = ad >>= (^? key "location" . key "address" . key "district" . key "name" . _String)
      area = (^?! key "target" . key "Area" . _String) <$> ad
      rooms = fromInteger <$> ((^?! key "property" . key "properties" . key "numberOfRooms" . _Integer) <$> ad)
      title = (^?! key "title" . _String) <$> ad
   in -- ppm = ad >>= (^? key "target" . key "Price_per_m" . _Integer)
      -- coordinates = (^?! key "location" . key "coordinates") <$> ad
      -- lat = coordinates >>= (^? key "latitude" . _Double)
      -- lon = coordinates >>= (^? key "longitude" . _Double)
      -- radius = (^? key "mapDetails" . key "radius") <$> coordinates
      do
        t <- title
        a <- parseNum area
        p <- fromInteger <$> price :: Maybe Int
        let defaultOffer =
              fromMaybe
                ( OfferView
                    { _offerURL = "",
                      _offerLatestPrice = p,
                      _offerArea = a,
                      _offerTitle = t,
                      _offerDetails = Nothing
                    }
                )
                offer
        let defaultDetails = fromMaybe emptyDetails (_offerDetails defaultOffer)
        let updateDetails d =
              d
                & (offerRooms .~ rooms)
                & (offerStreet .~ street)
                & (offerDistrict .~ district)
                & (offerDescription ?~ "updated here")

        ( over (non defaultOffer . offerTitle) (const t)
            . over (non defaultOffer . offerArea) (const a)
            . over (non defaultOffer . offerDetails . non defaultDetails) updateDetails
          )
          offer

detailsScraper :: Maybe OfferView -> Scraper Text OfferView
detailsScraper offer = do
  json <- Text.HTML.Scalpel.text $ "script" @: ["id" @= "__NEXT_DATA__"]
  let resultOffer = fromJSON json offer
  case resultOffer of
    Just o -> return o
    Nothing -> fail "parsing failed"

offersScraper :: Scraper Text [OfferView]
offersScraper = chroots ("div" @: ["data-cy" @= "search.listing.organic"] // "article") offerScraper

scraper :: WebScraper
-- scraper = prefixWebScraper "https://www.otodom.pl" (ScraperPack offersScraper Nothing)
scraper = prefixWebScraper "https://www.otodom.pl" (ScraperPack offersScraper (Just detailsScraper))
