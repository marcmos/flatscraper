module Scraper.MorizonScraper (scraper) where

import Control.Lens (element, (^?))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import DataAccess.ScrapeLoader (ScraperPack (ScraperPack), WebScraper, prefixWebScraper)
import Domain.Offer
  ( OfferDetails (_offerBuiltYear, _offerHasElevator, _offerPropertyFloor),
    OfferView (_offerDetails),
    emptyDetails,
    newOfferView,
    _offerBuildingFloors,
    _offerPropertyFloor,
    _offerRooms,
    _offerStreet,
  )
import Scraper.Common (parseDecimal, parseDouble, parsePrice)
import Text.HTML.Scalpel (Scraper, attr, chroots, hasClass, text, texts, (//), (@:), (@=))
import Text.Regex.TDFA (getAllTextSubmatches, (=~))

detailsScraper :: Maybe OfferView -> Scraper Text OfferView
detailsScraper offer = do
  locationText <- text $ "div" @: [hasClass "location-row__second_column"]

  let locationPat = "(.+, )+(.+)" :: Text
  let locationDetailed = Just $ last (getAllTextSubmatches (locationText =~ locationPat) :: [Text])

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
  let hasElev = find (== "Winda") boolAttrs

  case offer of
    Just o -> do
      let updatedDetails =
            (fromMaybe emptyDetails (_offerDetails o))
              { _offerStreet = locationDetailed,
                _offerBuiltYear = bYear,
                _offerHasElevator = if isJust hasElev then Just True else Nothing
              }
      return $ o {_offerDetails = Just updatedDetails}
    Nothing -> fail "FIXME: implement scrape details based only on direct details page scrape"

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
  rawPrice <- (^? element 2) <$> texts ("div" @: ["data-cy" @= "cardPropertyTopRight"] // "div")
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
            { -- (newOfferView parsedUrl p area $ T.pack . T.show $ detailsMatch)
              _offerDetails =
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
scraper = prefixWebScraper "https://www.morizon.pl" (ScraperPack offersScraper (Just detailsScraper))