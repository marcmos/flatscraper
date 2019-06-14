{-# LANGUAGE OverloadedStrings #-}

module OlxScraper
    ( olxScraper
    )
where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Data.Time
import Control.Monad ()

import Offer
import WordUtils

offerScraper :: UTCTime -> Scraper Text Offer
offerScraper timestamp = do
  name <- stripSpaces . dropShitwords <$> (text $ "a" // "strong")
  price <- parsePrice <$> text ("p" @: [hasClass "price"])
  url <- (attr "href" "a")
  return $ Offer name price Nothing url timestamp False "olx.pl"

detailsScraper :: Offer -> Scraper Text Offer
detailsScraper offer@(Offer {offerDetailed = True}) = return offer
detailsScraper offer = do
  attributes <- attrsScraper
  rent <- return $ do
    attribute <- Data.List.find (\x -> "Czynsz" `isInfixOf` (fst x)) attributes
    return . parsePrice . snd $ attribute
  return $ offer { offerDetailed = True
                 , offerRentPrice = rent
                 }

attrsScraper :: Scraper Text [(Text, Text)]
attrsScraper = chroot ("div" @: [hasClass "descriptioncontent"]) $ do
  offerAttrs <- chroots ("table" @: [hasClass "item"]) $ do
    k <- stripSpaces <$> text "th"
    v <- stripSpaces <$> text "td"
    return (k, v)
  return offerAttrs

offersScraper :: UTCTime -> Scraper Text [Offer]
offersScraper timestamp = chroots
  (("table" @: ["id" @= "offers_table"]) // "td" @: [hasClass "offer"])
  (offerScraper timestamp)

olxScraper :: OfferScraper
olxScraper = OfferScraper offersScraper (Just detailsScraper)
