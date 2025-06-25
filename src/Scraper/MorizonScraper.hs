{-# LANGUAGE LambdaCase #-}

module Scraper.MorizonScraper (scraper) where

import Control.Lens (element, (^?))
import Control.Monad (when)
import Data.IntMap.CharMap2 (update)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import DataAccess.ScrapeLoader
  ( ScraperPack (ScraperPack),
    WebScraper,
    prefixWebScraper,
  )
import Domain.Offer
  ( OfferDetails (_offerBuiltYear, _offerDistrict, _offerHasElevator, _offerMarket, _offerMunicipalityArea, _offerPropertyFloor),
    OfferMarket (MarketPrimary, MarketSecondary),
    OfferView (_offerDetails, _offerTitle),
    emptyDetails,
    emptyOffer,
    newOfferView,
    _offerBuildingFloors,
    _offerPropertyFloor,
    _offerRooms,
    _offerStreet,
  )
import Scraper.Common (findMap, parseDecimal, parseDouble, parsePrice)
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
import Text.Regex.TDFA (getAllTextSubmatches, (=~))

detailsScraper :: Maybe OfferView -> Scraper Text OfferView
detailsScraper offer = do
  title <- text $ "section" // "h1"
  locationText <- text $ "div" @: [hasClass "location-row__second_column"]

  let (street, municipality, district) = parseLocationText locationText

  -- itemValues <- texts $ "div" @: ["data-cy" @= "itemValue"]
  params <-
    chroots
      "div"
      ( do
          key <- text $ "div" // "span"
          value <- text $ "div" // ("div" @: ["data-cy" @= "itemValue"])
          return (key, value)
      )

  boolAttrs <-
    chroots "ul" $ do
      text ("li" // "span")

  let bYear = find (\(k, _) -> k == "Rok budowy") params >>= parseDecimal . snd
  let hasElev = find ("Winda" `T.isPrefixOf`) boolAttrs

  let market =
        findMap
          ( \case
              ("Rynek", "Pierwotny") -> Just MarketPrimary
              ("Rynek", "Wtórny") -> Just MarketSecondary
              _ -> Nothing
          )
          params

  let newOffer = fromMaybe emptyOffer offer
  let updatedDetails =
        (fromMaybe emptyDetails (_offerDetails newOffer))
          { _offerStreet = street,
            _offerDistrict = district,
            _offerMunicipalityArea = municipality,
            _offerBuiltYear = bYear,
            _offerHasElevator =
              if isJust hasElev
                then Just True
                else Nothing,
            _offerMarket = market
          }

  return $
    newOffer
      { _offerTitle = title,
        _offerDetails = Just updatedDetails
      }

parseFloors :: Text -> Maybe (Int, Maybe Int)
parseFloors t =
  let floorPat = "(piętro ([0-9]+)|parter)(/([0-9]+))?" :: Text
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

parseArea :: Text -> Maybe Double
parseArea t = do
  let areaPat = "([0-9]+) m²" :: Text
  case getAllTextSubmatches (t =~ areaPat) :: [Text] of
    [_, areaText] -> parseDouble areaText
    _ -> Nothing

parseRooms :: Text -> Maybe Int
parseRooms t =
  let roomPat = "([1-9]) pok" :: Text
   in case getAllTextSubmatches (t =~ roomPat) :: [Text] of
        [_, roomText] -> parseDecimal roomText
        _ -> Nothing

listOfferScraper :: Scraper Text OfferView
listOfferScraper = do
  url <- attr "href" $ "a" @: ["data-cy" @= "propertyUrl"]
  let parsedUrl = "https://www.morizon.pl" <> url
  rawPrice <-
    (^? element 2)
      <$> texts ("div" @: ["data-cy" @= "cardPropertyOfferPrice"] // "div")
  let price = parsePrice <$> rawPrice
  -- title <- text "h4"
  detailsTexts <- text ("div" @: [hasClass "property-info"])
  -- detailsTexts <- return "52 m², parter/10"
  let rooms = parseRooms detailsTexts
  let offer = do
        p <- price
        area <- parseArea detailsTexts
        let floorInfo = parseFloors detailsTexts
        return $
          (newOfferView parsedUrl p area detailsTexts)
            { _offerDetails =
                Just
                  ( emptyDetails
                      { _offerRooms = rooms,
                        _offerPropertyFloor = fst <$> floorInfo,
                        _offerBuildingFloors = floorInfo >>= snd
                      }
                  )
            }
  case offer of
    Just o -> return o
    Nothing -> fail "z"

offersScraper :: Scraper Text [OfferView]
offersScraper = do
  chroots ("div" @: ["data-cy" @= "card"]) listOfferScraper

scraper :: WebScraper
scraper =
  prefixWebScraper
    "https://www.morizon.pl"
    ( ScraperPack
        offersScraper
        (Just detailsScraper)
    )