{-# LANGUAGE OverloadedStrings #-}

module GumtreeScraper
  ( gumtreeScraper
  ) where

import Text.HTML.Scalpel
import Data.Text (Text)
import Data.Maybe (Maybe(Nothing))
import Data.Time (UTCTime)

import Offer
import WordUtils

offerScraper :: UTCTime -> Scraper Text Offer
offerScraper timestamp = do
  let titleElement = "div" @: [hasClass "title"]
  name <- stripSpaces . dropShitwords <$> (text $ titleElement)
  url <- ("https://www.gumtree.pl" <>) <$> attr "href" (titleElement // "a")
  price <- parsePrice <$> text ("span" @: [hasClass "ad-price"])
  return $ Offer name price Nothing url timestamp False "gumtree.pl"

offersScraper :: UTCTime -> Scraper Text [Offer]
offersScraper timestamp = chroots
  ("div" @: [hasClass "view"] // "div" @: [hasClass "tileV1"])
  (offerScraper timestamp)

gumtreeScraper :: OfferScraper
gumtreeScraper = OfferScraper offersScraper Nothing
