module Scraper.NieruchOnlineScraper (scraper) where

import Control.Lens ((^?))
import Control.Lens.Combinators (element)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, isSuffixOf, unpack)
import DataAccess.ScrapeLoader (ScraperPack (..), WebScraper, prefixWebScraper)
import Domain.Offer
  ( OfferDetails
      ( _offerBuildingFloors,
        _offerDistrict,
        _offerMunicipalityArea,
        _offerPropertyFloor,
        _offerRooms
      ),
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
  let updatedDetails =
        (fromMaybe emptyDetails (_offerDetails ov))
          { _offerStreet = street,
            _offerDistrict = district,
            _offerMunicipalityArea = municipality,
            _offerPropertyFloor = fst <$> floorInfo,
            _offerBuildingFloors = floorInfo >>= snd,
            _offerRooms = mapFind parseRooms attrsTable
          }
  return $ ov {_offerDetails = Just updatedDetails}
offerDetailsScraper Nothing = fail "not implemented"

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
