module Scraper.NieruchOnlineScraper (scraper) where

import Control.Lens ((^?))
import Control.Lens.Combinators (element)
import Data.Aeson (Value, decodeStrict)
import Data.Aeson.Lens
  ( AsNumber (_Integer),
    key,
    _String,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, isSuffixOf, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import DataAccess.ScrapeLoader (ScraperPack (..), WebScraper, prefixWebScraper)
import Domain.Offer
  ( OfferCoordinates (..),
    OfferDetails
      ( _offerBuildingFloors,
        _offerCoordinates,
        _offerDistrict,
        _offerMarket,
        _offerMunicipalityArea,
        _offerPropertyFloor,
        _offerRooms
      ),
    OfferMarket (MarketPrimary, MarketSecondary),
    OfferView
      ( _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    emptyDetails,
    emptyOffer,
    _offerStreet,
  )
import Scraper.Common (parseDecimal, parseDouble, parsePrice')
import Scraper.Dictionary (parseLocationText)
import Text.HTML.Scalpel
  ( Scraper,
    attr,
    chroots,
    hasClass,
    text,
    texts,
    (//),
    (@:),
    (@=),
  )
import Text.Regex.TDFA (AllTextSubmatches (..), (=~))

scraper :: WebScraper
scraper =
  prefixWebScraper
    (T.unpack citySubpageURL)
    ( ScraperPack
        offerListScraper
        (Just offerDetailsScraper)
    )

citySubpageURL :: Text
citySubpageURL = "https://krakow.nieruchomosci-online.pl"

mapFind :: (a -> Maybe b) -> [a] -> Maybe b
mapFind _ [] = Nothing
mapFind f (x : xs) = case f x of
  Just y -> Just y
  Nothing -> mapFind f xs

parseFloors :: Text -> Maybe (Int, Maybe Int)
parseFloors t =
  let floorPat = "(Piętro:([0-9]+)|parter)(/([0-9]+))?" :: Text
      detailsMatch = getAllTextSubmatches (t =~ floorPat) :: [Text]
   in case detailsMatch of
        [_, "parter", _, _, lastFloorText] -> do
          lastFloor <- parseDecimal lastFloorText
          Just (0, Just lastFloor)
        [_, _, floorText, "", ""] -> do
          apFloor <- parseDecimal floorText
          Just (apFloor, Nothing)
        [_, _, floorText, _, lastFloorText] -> do
          apFloor <- parseDecimal floorText
          lastFloor <- parseDecimal lastFloorText
          Just (apFloor, Just lastFloor)
        _ -> Nothing

parseRooms :: Text -> Maybe Int
parseRooms t =
  let roomPat = "Liczba pokoi:([1-9])" :: Text
   in case getAllTextSubmatches (t =~ roomPat) :: [Text] of
        [_, roomText] -> parseDecimal roomText
        _ -> Nothing

offerDetailsScraper :: Maybe OfferView -> Scraper Text OfferView
offerDetailsScraper (Just ov) = do
  locationText <- text $ "ul" @: ["id" @= "locationUl"] // "span"
  let (street, municipality, district) = parseLocationText locationText
  attrsTable <- texts $ "div" @: ["id" @= "attributesTable"] // "div"
  let floorInfo = mapFind parseFloors attrsTable
  detailsTable <- texts $ "div" @: ["id" @= "detailsTable"] // "li"
  let market =
        mapFind
          ( \t -> case ( getAllTextSubmatches
                           ( t
                               =~ ("Rynek: (pierwotny|wtórny)" :: Text)
                           ) ::
                           [Text]
                       ) of
              [_, "pierwotny"] -> Just MarketPrimary
              [_, "wtórny"] -> Just MarketSecondary
              _ -> Nothing
          )
          detailsTable
  scripts <- texts "script"
  let matches = map (\t -> getAllTextSubmatches (t =~ ("var mapInitData = (.+);" :: Text)) :: [Text]) scripts
  let locationJsonText = mapFind (\t -> if null t then Nothing else t ^? element 1) matches
  let json = (locationJsonText >>= decodeStrict . T.encodeUtf8) :: Maybe Value
  let coordinates = do
        obj <- json
        latText <- obj ^? key "latitude" . _String
        lonText <- obj ^? key "longitude" . _String
        inaccurate <- obj ^? key "isInaccurateLocation" . _Integer
        lat <- parseDouble latText
        lon <- parseDouble lonText
        if inaccurate == 0
          then return $ OfferExactCoordinates lat lon
          else Nothing
  let updatedDetails =
        (fromMaybe emptyDetails (_offerDetails ov))
          { _offerStreet = street,
            _offerDistrict = district,
            _offerMunicipalityArea = municipality,
            _offerPropertyFloor = fst <$> floorInfo,
            _offerBuildingFloors = floorInfo >>= snd,
            _offerRooms = mapFind parseRooms attrsTable,
            _offerCoordinates = coordinates,
            _offerMarket = market
          }
  return $ ov {_offerDetails = Just updatedDetails}
offerDetailsScraper Nothing = offerDetailsScraper $ Just emptyOffer

offerItemScraper :: Scraper Text OfferView
offerItemScraper = do
  title <- text $ "h2" @: [hasClass "name"]
  url <- attr "href" $ "h2" @: [hasClass "name"] // "a"
  t <- texts $ "div" @: [hasClass "tertiary__box--primary"] // "span"
  let offer = do
        price <- t ^? element 0 >>= parsePrice'
        area <- t ^? element 1 >>= parseDouble
        return
          emptyOffer
            { _offerTitle = title,
              _offerLatestPrice = price,
              _offerArea = area,
              _offerURL = url
            }
  case offer of
    Just o -> return o
    Nothing -> fail "Failed to parse offer"

offerListScraper :: Scraper Text [OfferView]
offerListScraper = do
  offers <-
    chroots
      ("section" @: [hasClass "tiles"] // "div" @: [hasClass "tile"])
      offerItemScraper

  return $
    filter
      ( \o ->
          -- Filter out offers for other cities by checking if the URL starts
          -- with the city subpage URL.
          citySubpageURL `T.isPrefixOf` _offerURL o
            -- Filter out investments -- having non-empty query param at the end
            -- of the URL.
            && not ("?" `T.isSuffixOf` _offerURL o)
      )
      offers
