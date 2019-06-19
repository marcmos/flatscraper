{-# LANGUAGE OverloadedStrings #-}

module OlxScraper
    ( olxScraper
    )
where

import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad ()

import Offer
import WordUtils

offerScraper :: BasicOffer -> Scraper Text Offer
offerScraper offer = do
  name <- stripSpaces . dropShitwords <$> (text $ "a" // "strong")
  price <- parsePrice <$> text ("p" @: [hasClass "price"])
  url <- (attr "href" "a")
  return $ offer name price url

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

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer = chroots
  (("table" @: ["id" @= "offers_table"]) // "td" @: [hasClass "offer"])
  (offerScraper offer)

olxScraper :: Config Text -> OfferScraper
olxScraper config = OfferScraper
  config
  (basicOffer "olx.pl")
  offersScraper
  (Just detailsScraper)
