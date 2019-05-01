{-# LANGUAGE OverloadedStrings #-}

module OtodomScraper
    ( scrapOtodomOffers
    , flatScrapOtodomOffers
    ) where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad ()

import Offer
import WordUtils

offerScraper :: Scraper Text Offer
offerScraper = do
  name <- stripSpaces . dropShitwords <$> text ("span" @: [hasClass "offer-item-title"])
  price <- stripSpaces <$> text ("li" @: [hasClass "offer-item-price"])
  url <- attr "href" "a"
  return $ Offer name price Nothing url Nothing

rentScraper :: Scraper Text [Text]
rentScraper = texts ("section" @: [hasClass "section-overview"] //
                     "div" // "ul" // "li")

extractRentPrice :: Text -> IO (Maybe Text)
extractRentPrice url = do
  scraped <- scrapeURL (unpack url) rentScraper
  return $ scraped >>= Data.List.find ("Czynsz" `isInfixOf`)

offersScraper :: Scraper Text [Offer]
offersScraper = chroots
  ("article" @: [hasClass "offer-item", "data-featured-name" @= "listing_no_promo"])
  offerScraper

augmentByOfferRentPrice :: Offer -> IO Offer
augmentByOfferRentPrice offer = do
  price <- extractRentPrice $ offerURL offer
  return $ case price of
    Just p -> offer {offerRentPriceStr = Just p}
    Nothing    -> offer

flatScrapOtodomOffers :: String -> IO (Maybe [Offer])
flatScrapOtodomOffers url = scrapeURL url offersScraper

scrapOtodomOffers :: String -> IO (Maybe [Offer])
scrapOtodomOffers url = do
  x <- flatScrapOtodomOffers url
  case x of
    Just o -> Just <$> mapM augmentByOfferRentPrice o
    Nothing -> return Nothing
