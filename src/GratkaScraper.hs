{-# LANGUAGE OverloadedStrings #-}

module GratkaScraper
    ( gratkaScraper
    ) where

import Text.HTML.Scalpel
import Data.Text (Text, isInfixOf)
import Data.List (find)
import Data.Time

import Offer
import WordUtils

offerScraper :: UTCTime -> Scraper Text Offer
offerScraper timestamp = do
  name <- stripSpaces . dropShitwords <$> text ("h2" @: [hasClass "teaser__title"])
  price <- text ("p" @: [hasClass "teaser__price"])
  rentPrice <- Data.List.find ("czynsz" `isInfixOf`) <$> texts
    ("ul" @: [hasClass "teaser__params"] // "li")
  url <- attr "data-href" "article"
  return $ Offer name (parsePrice price) (parsePrice <$> rentPrice) url timestamp True "gratka.pl"

offersScraper :: UTCTime -> Scraper Text [Offer]
offersScraper timestamp = chroots ("article" @: [hasClass "teaser"]) (offerScraper timestamp)

gratkaScraper :: Config Text -> OfferScraper
gratkaScraper config = OfferScraper config offersScraper Nothing
