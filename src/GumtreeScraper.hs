{-# LANGUAGE OverloadedStrings #-}

module GumtreeScraper
  ( gumtreeScraper
  , detailedOfferScraper
  ) where

import Text.HTML.Scalpel
import Data.Text (Text, isInfixOf)

import Offer
import WordUtils

attrsScraper :: Scraper Text [(Text, Text)]
attrsScraper = chroot ("div" @: [hasClass "vip-details"]) $
  do chroots "li"
     $ do k <- stripSpaces <$> text ("span" @: [hasClass "name"])
          v <- stripSpaces <$> text ("span" @: [hasClass "value"])
          return (k, v)

detailedOfferScraper :: Offer -> Scraper Text Offer
detailedOfferScraper offer = chroot ("div" @: [hasClass "vip-details"]) $ do
  offerAttrs <- attrsScraper
  let direct = (== "Właściciel") <$> lookup "Do wynajęcia przez" offerAttrs
  let rooms = lookup "Liczba pokoi" offerAttrs >>= parseRooms
  let area = lookup "Wielkość (m2)" offerAttrs >>= parseInt
  description <- text ("div" @: [hasClass "description"])
  let extras = parseExtras description
  return offer { offerOwnerOffer = direct
               , offerExtras = extras
               , offerDetailed = True
               , offerRooms = rooms
               , offerArea = area
               , offerDescription = Just description
               }

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
