module Scraper.OlxScraper
  ( scraper,
  )
where

import Control.Lens (element, (^?))
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, replace, stripSuffix, takeWhile)
import qualified Data.Text.Read as T (double)
import DataAccess.ScrapeLoader
  ( ScraperPack (ScraperPack),
    WebScraper,
    prefixWebScraper,
  )
import Domain.Offer
  ( OfferDetails
      ( _offerPropertyFloor,
        _offerRooms
      ),
    OfferView (_offerDetails),
    emptyDetails,
    newOfferView,
  )
import Scraper.Common (parseDecimal, parsePrice')
import Text.HTML.Scalpel (Scraper, attr, chroots, text, texts, (//), (@:), (@=))
import Text.Regex.TDFA (getAllTextSubmatches, (=~))

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap _ [] = Nothing
findMap f (x : xs) = case f x of
  Just y -> Just y
  Nothing -> findMap f xs

detailsScraper :: Maybe OfferView -> Scraper Text OfferView
detailsScraper (Just ov) = do
  attrsTexts <- texts $ "div" @: ["data-testid" @= "ad-parameters-container"] // "p"
  let findLevel :: Text -> Maybe Int
      findLevel t = case getAllTextSubmatches
        (t =~ ("Poziom: (Parter|[1-9])" :: Text)) of
        [_, "Parter"] -> Just 0
        [_, levelText] -> parseDecimal levelText
        _ -> Nothing
      findRooms :: Text -> Maybe Int
      findRooms t = case getAllTextSubmatches
        (t =~ ("Liczba pokoi: ([1-9])" :: Text)) of
        [_, roomsText] -> parseDecimal roomsText
        _ -> Nothing
      updatedDetails =
        (fromMaybe emptyDetails (_offerDetails ov))
          { _offerPropertyFloor = findMap findLevel attrsTexts,
            _offerRooms = findMap findRooms attrsTexts
          }
  return $ ov {_offerDetails = Just updatedDetails}
detailsScraper Nothing = fail ""

listOfferScraper :: Scraper Text OfferView
listOfferScraper = do
  url <- attr "href" "a"
  let parsedUrl =
        if "https://www.otodom.pl" `T.isPrefixOf` url
          then -- otodom urls on the olx side are ending with ".html" whereas on the
          -- otodom site they are not; let's normalize them by stripping the
          -- ".html" suffix to avoid duplicates at the later stage
            fromMaybe url $ T.stripSuffix ".html" url
          else "https://www.olx.pl" <> url
  rawPrice <- listToMaybe <$> texts ("p" @: ["data-testid" @= "ad-price"])
  title <- text "h4"
  area <- (^? element 2) <$> texts "span"
  let offer = do
        p <- rawPrice
        price <- parsePrice' p
        area' <-
          rightToMaybe
            . T.double
            . T.replace "," "."
            . T.takeWhile (/= ' ')
            <$> area
        area'' <- fst <$> area'
        return $ newOfferView parsedUrl price area'' title
  case offer of
    Just o -> return o
    Nothing -> fail "z"

offersScraper :: Scraper Text [OfferView]
offersScraper = do
  chroots
    ( "div" @: ["data-testid" @= "listing-grid"]
        // "div"
          @: ["data-testid" @= "l-card"]
    )
    listOfferScraper

scraper :: WebScraper
scraper =
  prefixWebScraper
    "https://www.olx.pl"
    ( ScraperPack
        offersScraper
        (Just detailsScraper)
    )