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
    (^..),
    (^?),
    (^?!),
  )
import Control.Lens.Lens ((<&>))
import Control.Monad ()
import Data.Aeson (Value (), decodeStrict)
import Data.Aeson.Lens
  ( AsNumber (_Integer),
    AsValue (_Array),
    key,
    values,
    _JSON,
    _String,
  )
import Data.Either.Combinators (rightToMaybe)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Text.Lens ()
import qualified Data.Text.Read as T (decimal, double)
import Data.Vector (toList)
import DataAccess.ScrapeLoader
  ( ScraperPack (ScraperPack),
    WebScraper,
    prefixWebScraper,
  )
import Domain.Offer
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
    offerBuildingFloors,
    offerBuiltYear,
    offerDescription,
    offerDetails,
    offerDistrict,
    offerHasElevator,
    offerMunicipalityArea,
    offerPropertyFloor,
    offerRooms,
    offerStreet,
    offerTitle,
  )
import Scraper.Common (parsePrice)
import Text.HTML.Scalpel (Scraper, attr, chroots, text, texts, (//), (@:), (@=))

offerScraper :: Scraper Text OfferView
offerScraper = do
  name <- text ("p" @: ["data-cy" @= "listing-item-title"])
  price <-
    parsePrice . fromMaybe "0 zł" . find (T.isSuffixOf "zł")
      <$> texts "span"
  url <- ("https://www.otodom.pl" <>) <$> attr "href" "a"
  return $ newOfferView url price 0.0 name

parseNum :: Maybe Text -> Maybe Double
parseNum x = do
  a <- x
  b <- rightToMaybe $ T.double a
  return $ fst b

parseInt :: Text -> Maybe Int
parseInt t = do
  i <- rightToMaybe $ T.decimal t
  return $ fst i

parseLocation :: Value -> Maybe [Text]
parseLocation locationObj = do
  obj <-
    locationObj
      ^? key "reverseGeocoding"
        . key "locations"
        . _Array
  let reversed = reverse (toList obj)
  detailed <- listToMaybe reversed
  fullNameItems <- detailed ^? key "fullNameItems" . _Array
  traverse (^? _String) (toList fullNameItems)

fromJSON :: Text -> Maybe OfferView -> Maybe OfferView
fromJSON input offer =
  let bs = T.encodeUtf8 input
      json = decodeStrict bs :: Maybe Value
      ad = (^?! key "props" . key "pageProps" . key "ad") <$> json
      price = ad >>= (^? key "target" . key "Price" . _Integer)
      street =
        ad
          >>= ( ^?
                  key "location"
                    . key "address"
                    . key "street"
                    . key "name"
                    . _String
              )
      -- district =
      --   ad
      --     >>= ( ^?
      --             key "location"
      --               . key "address"
      --               . key "district"
      --               . key "name"
      --               . _String
      --         )
      area = (^?! key "target" . key "Area" . _String) <$> ad
      properties = ad >>= (^? key "property" . key "properties")
      rooms = fromInteger <$> (properties >>= (^? key "numberOfRooms" . _Integer))
      propertyFloor = case properties >>= (^? key "floor" . _String) of
        Just "GROUND_FLOOR" -> Just 0
        Just floorText -> T.stripPrefix "FLOOR_" floorText >>= parseInt
        Nothing -> Nothing
      title = (^?! key "title" . _String) <$> ad
      buildingProperties = ad >>= (^? key "property" . key "buildingProperties")
      builtYear =
        fromInteger
          <$> ( buildingProperties
                  >>= ( ^?
                          key "year"
                            . _Integer
                      )
              )
      buildingFloors =
        fromInteger
          <$> ( buildingProperties
                  >>= (^? key "numberOfFloors" . _Integer)
              )
      conveniences =
        (^.. values)
          <$> ( buildingProperties
                  >>= (^? key "conveniences")
              )
      hasLift = elem "LIFT" <$> conveniences
      location = ad >>= (^? key "location")
      (muniArea, district) = case location >>= parseLocation of
        Just [a, d, "Kraków", "małopolskie"] ->
          (Just a, Just d)
        _ -> (Nothing, Nothing)
      equipment = (^.. values) <$> (properties >>= (^? key "equipment"))
      hasAirConditioning = elem "air_conditioning" <$> equipment
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
                & (offerBuiltYear .~ builtYear)
                & (offerBuildingFloors .~ buildingFloors)
                & (offerPropertyFloor .~ propertyFloor)
                & (offerHasElevator .~ hasLift)
                & (offerMunicipalityArea .~ muniArea)

        ( over (non defaultOffer . offerTitle) (const t)
            . over (non defaultOffer . offerArea) (const a)
            . over
              (non defaultOffer . offerDetails . non defaultDetails)
              updateDetails
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
offersScraper = do
  offers <-
    chroots
      ( "div" @: ["data-cy" @= "search.listing.organic"]
          // "article"
      )
      offerScraper
  -- Sometimes it returns duplicated offers prefixed with "/hpr/". It looks like
  -- that when it happens, there's a second offer with the same URL but without
  -- the prefix. Let's filter them out.
  return $
    filter
      (not . T.isPrefixOf "https://www.otodom.pl/hpr/" . _offerURL)
      offers

scraper :: WebScraper
scraper =
  prefixWebScraper
    "https://www.otodom.pl"
    ( ScraperPack
        offersScraper
        (Just detailsScraper)
    )
