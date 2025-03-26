{-# LANGUAGE OverloadedStrings #-}

module Scraper.GratkaScraper
    ( gratkaScraper
    ) where

import Text.HTML.Scalpel
import Data.Text (Text, isInfixOf)
import Data.List (find)

import Offer
import WordUtils

offerScraper :: BasicOffer -> Scraper Text Offer
offerScraper offer = do
  name <- stripSpaces . dropShitwords <$> text ("h2" @: [hasClass "teaser__title"])
  price <- text ("p" @: [hasClass "teaser__price"])
  rentPrice <- Data.List.find ("czynsz" `isInfixOf`) <$> texts
    ("ul" @: [hasClass "teaser__params"] // "li")
  url <- attr "data-href" "article"
  return $ (offer name (parsePrice price) url)
    { offerRentPrice = parsePrice <$> rentPrice
    , offerDetailed = True
    }

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer = chroots ("article" @: [hasClass "teaser"]) (offerScraper offer)

gratkaScraper :: Config Text -> OfferScraper
gratkaScraper config = OfferScraper
  config
  (basicOffer "gratka.pl")
  offersScraper
  Nothing
