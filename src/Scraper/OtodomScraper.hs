{-# LANGUAGE OverloadedStrings #-}

module Scraper.OtodomScraper
  ( offersScraper,
    detailsScraper,
  )
where

import Control.Lens ((^?), (^?!))
import Control.Monad ()
import Data.Aeson (Value (), decodeStrict)
import Data.Aeson.Lens (AsNumber (_Integer), key, _String)
import Data.Char (isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, any, concat, isSuffixOf, takeWhile, words)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Text.Lens ()
import qualified Data.Text.Read as T (decimal, double)
import Text.HTML.Scalpel (Scraper, attr, chroots, text, texts, (//), (@:), (@=))
import UseCase.Offer
  ( OfferDetails (OfferDetails, offerDescription, offerDistrict, offerRooms, offerStreet),
    OfferView
      ( offerArea,
        offerDetails,
        offerTitle
      ),
    newOfferView,
  )

parsePrice :: Text -> Int
parsePrice x =
  either (const 0) fst ((T.decimal . T.takeWhile (',' /=) . T.concat . (filter (T.any isDigit) <$> T.words)) x)

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

fromJSON :: Text -> OfferView -> Maybe OfferView
fromJSON input offer =
  let bs = T.encodeUtf8 input
      json = decodeStrict bs :: Maybe Value
      ad = (^?! key "props" . key "pageProps" . key "ad") <$> json
      price = ad >>= (^? key "target" . key "Price" . _Integer)
      street = ad >>= (^? key "location" . key "address" . key "street" . key "name" . _String)
      district = ad >>= (^? key "location" . key "address" . key "district" . key "name" . _String)
      area = (^?! key "target" . key "Area" . _String) <$> ad
      rooms = (^?! key "property" . key "properties" . key "numberOfRooms" . _Integer) <$> ad
      title = (^?! key "title" . _String) <$> ad
   in -- ppm = ad >>= (^? key "target" . key "Price_per_m" . _Integer)
      -- coordinates = (^?! key "location" . key "coordinates") <$> ad
      -- lat = coordinates >>= (^? key "latitude" . _Double)
      -- lon = coordinates >>= (^? key "longitude" . _Double)
      -- radius = (^? key "mapDetails" . key "radius") <$> coordinates
      do
        t <- title
        a <- parseNum area
        let r = fromInteger <$> rooms
        return $ case offerDetails offer of
          Just o ->
            offer
              { offerDetails =
                  Just
                    o
                      { offerRooms = r,
                        offerStreet = street,
                        offerDistrict = district
                      },
                offerArea = a
              }
          Nothing ->
            offer
              { offerArea = a,
                offerDetails =
                  Just
                    OfferDetails
                      { offerRooms = r,
                        offerDescription = Nothing,
                        offerStreet = street,
                        offerDistrict = district
                      }
              }

-- offer
--   { offerArea = parseNum area,
--     offerLocation = OfferLocation district . Just <$> street,
--     offerRooms = fromInteger <$> rooms,
--     offerTitle = t,
--     offerPrice = fromInteger p
--   }

detailsScraper :: OfferView -> Scraper Text OfferView
detailsScraper offer = do
  json <- Text.HTML.Scalpel.text $ "script" @: ["id" @= "__NEXT_DATA__"]
  return $ fromMaybe offer (fromJSON json offer)

offersScraper :: Scraper Text [OfferView]
offersScraper = chroots ("div" @: ["data-cy" @= "search.listing.organic"] // "article") offerScraper

-- otodomScraper :: Config Text -> OfferScraper
-- otodomScraper config =
--   OfferScraper
--     config
--     (basicOffer "otodom.pl")
--     offersScraper
--     (Just detailsScraper)
