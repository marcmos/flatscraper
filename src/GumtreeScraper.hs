{-# LANGUAGE OverloadedStrings #-}

module GumtreeScraper
  ( gumtreeScraper
  ) where

import Text.HTML.Scalpel
import Data.Text (Text)

import Offer
import WordUtils

detailedOfferScraper :: Offer -> Scraper Text Offer
detailedOfferScraper offer = chroot ("div" @: [hasClass "vip-details"]) $ do
  offerAttrs <- texts ("div" @: [hasClass "attribute"] // "span" @: [hasClass "value"])
  direct <- return . Just $ (not . elem "Agencja") (stripSpaces <$> offerAttrs)
  return offer { offerDirect = direct }

listOfferScraper :: BasicOffer -> Scraper Text Offer
listOfferScraper offer = do
  let titleElement = "div" @: [hasClass "title"]
  name <- stripSpaces . dropShitwords <$> (text $ titleElement)
  url <- ("https://www.gumtree.pl" <>) <$> attr "href" (titleElement // "a")
  price <- parsePrice <$> text ("span" @: [hasClass "ad-price"])
  return $ offer name price url

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer = chroots
  ("div" @: [hasClass "view"] // "div" @: [hasClass "tileV1"])
  (listOfferScraper offer)

gumtreeScraper :: Config Text -> OfferScraper
gumtreeScraper config = OfferScraper
  config
  (basicOffer "gumtree.pl")
  offersScraper
  (Just detailedOfferScraper)
