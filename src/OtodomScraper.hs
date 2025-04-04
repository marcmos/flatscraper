{-# LANGUAGE OverloadedStrings #-}

module OtodomScraper
    ( otodomScraper
    ) where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad ()

import Offer
import WordUtils

offerScraper :: BasicOffer -> Scraper Text Offer
offerScraper offer = do
  name <- stripSpaces . dropShitwords <$> text ("span" @: [hasClass "offer-item-title"])
  price <- parsePrice <$> text ("li" @: [hasClass "offer-item-price"])
  url <- attr "href" "a"
  return $ offer name price url

detailsScraper :: Offer -> Scraper Text Offer
detailsScraper offer@(Offer {offerDetailed = True}) = return offer
detailsScraper offer = do
  offerAttrs <- texts ("section" @: [hasClass "section-overview"] //
                       "div" // "ul" // "li")
  rent <- return $ parsePrice <$> Data.List.find ("Czynsz" `isInfixOf`) offerAttrs
  return $ case rent of
    Just r  -> offer { offerRentPrice = Just r, offerDetailed = True }
    Nothing -> offer

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer = chroots
  ("article" @: [hasClass "offer-item", "data-featured-name" @= "listing_no_promo"])
  (offerScraper offer)

otodomScraper :: Config Text -> OfferScraper
otodomScraper config = OfferScraper
  config
  (basicOffer "otodom.pl")
  offersScraper
  (Just detailsScraper)
