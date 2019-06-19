{-# LANGUAGE OverloadedStrings #-}

module GumtreeScraper
  ( gumtreeScraper
  ) where

import Text.HTML.Scalpel
import Data.Text (Text)
import Data.Maybe (Maybe(Nothing))

import Offer
import WordUtils

offerScraper :: BasicOffer -> Scraper Text Offer
offerScraper offer = do
  let titleElement = "div" @: [hasClass "title"]
  name <- stripSpaces . dropShitwords <$> (text $ titleElement)
  url <- ("https://www.gumtree.pl" <>) <$> attr "href" (titleElement // "a")
  price <- parsePrice <$> text ("span" @: [hasClass "ad-price"])
  return $ offer name price url

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer = chroots
  ("div" @: [hasClass "view"] // "div" @: [hasClass "tileV1"])
  (offerScraper offer)

gumtreeScraper :: Config Text -> OfferScraper
gumtreeScraper config = OfferScraper
  config
  (basicOffer "gumtree.pl")
  offersScraper
  Nothing
