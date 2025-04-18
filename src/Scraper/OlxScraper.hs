{-# LANGUAGE OverloadedStrings #-}

module Scraper.OlxScraper
  ( scraper,
  )
where

import Control.Lens (element, (^?))
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, replace, takeWhile)
import qualified Data.Text.Read as T (double)
import DataAccess.ScrapeLoader (ScraperPack (ScraperPack), WebScraper, prefixWebScraper)
import Scraper.Common (parsePrice')
import Text.HTML.Scalpel (Scraper, attr, chroots, text, texts, (//), (@:), (@=))
import UseCase.Offer (OfferView, newOfferView)

listOfferScraper :: Scraper Text OfferView
listOfferScraper = do
  url <- attr "href" "a"
  let parsedUrl = if "https://www.otodom.pl" `T.isPrefixOf` url then url else "https://www.olx.pl" <> url
  rawPrice <- listToMaybe <$> texts ("p" @: ["data-testid" @= "ad-price"])
  title <- text "h4"
  area <- (^? element 2) <$> texts "span"
  let offer = do
        p <- rawPrice
        price <- parsePrice' p
        area' <- rightToMaybe . T.double . T.replace "," "." . T.takeWhile (/= ' ') <$> area
        area'' <- fst <$> area'
        return $ newOfferView parsedUrl price area'' title
  case offer of
    Just o -> return o
    Nothing -> fail "z"

offersScraper :: Scraper Text [OfferView]
offersScraper = do
  chroots ("div" @: ["data-testid" @= "listing-grid"] // "div" @: ["data-testid" @= "l-card"]) listOfferScraper

scraper :: WebScraper
scraper = prefixWebScraper "https://www.olx.pl" (ScraperPack offersScraper Nothing)