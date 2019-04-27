{-# LANGUAGE OverloadedStrings #-}

module GratkaScraper
    ( scrapGratkaOffers
    ) where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)

import Offer (Offer(..))
import WordUtils

offerScraper :: Scraper Text Offer
offerScraper = do
  name <- stripSpaces . dropShitwords <$> text ("h2" @: [hasClass "teaser__title"])
  price <- stripSpaces <$> text ("p" @: [hasClass "teaser__price"])
  rentPrice <- Data.List.find ("czynsz" `isInfixOf`) <$> texts
    ("ul" @: [hasClass "teaser__params"] // "li")
  url <- attr "data-href" "article"
  return $ Offer name price rentPrice url

offersScraper :: Scraper Text [Offer]
offersScraper = chroots ("article" @: [hasClass "teaser"]) offerScraper

scrapGratkaOffers :: String -> IO (Maybe [Offer])
scrapGratkaOffers url = do
  offers <- scrapeURL url offersScraper
  return $ case offers of
    Just offer -> Just offer
    Nothing -> Nothing
