{-# LANGUAGE OverloadedStrings #-}

module OtodomScraper
    ( otodomScraper
    ) where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Data.Time
import Control.Monad ()

import Offer
import WordUtils

offerScraper :: UTCTime -> Scraper Text Offer
offerScraper timestamp = do
  name <- stripSpaces . dropShitwords <$> text ("span" @: [hasClass "offer-item-title"])
  price <- parsePrice <$> text ("li" @: [hasClass "offer-item-price"])
  url <- attr "href" "a"
  return $ Offer name price Nothing url timestamp False "otodom.pl"

detailsScraper :: Offer -> Scraper Text Offer
detailsScraper offer@(Offer {offerDetailed = True}) = return offer
detailsScraper offer = do
  offerAttrs <- texts ("section" @: [hasClass "section-overview"] //
                       "div" // "ul" // "li")
  rent <- return $ parsePrice <$> Data.List.find ("Czynsz" `isInfixOf`) offerAttrs
  return $ case rent of
    Just r  -> offer { offerRentPrice = Just r, offerDetailed = True }
    Nothing -> offer

offersScraper :: UTCTime -> Scraper Text [Offer]
offersScraper timestamp = chroots
  ("article" @: [hasClass "offer-item", "data-featured-name" @= "listing_no_promo"])
  (offerScraper timestamp)

otodomScraper :: OfferScraper
otodomScraper = OfferScraper offersScraper (Just detailsScraper)
