{-# LANGUAGE OverloadedStrings #-}

module Legacy.Scraper.OlxScraper
  ( olxScraper,
  )
where

import Control.Monad ()
import Data.List (find, lookup)
import Data.Maybe (isJust)
import Data.Text as T
import Legacy.Domain.Offer
import Legacy.Domain.WordUtils
import Text.HTML.Scalpel

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
    attribute <- Data.List.find (\x -> "Czynsz (dodatkowo)" `isInfixOf` (fst x)) attributes
    return . parsePrice . snd $ attribute
  area <- return $ do
    attribute <- Data.List.find (\x -> "Powierzchnia" `isInfixOf` (fst x)) attributes
    return . parsePrice . snd $ attribute
  private <- return $ (isJust $ Data.List.find (\x -> "Osoby prywatnej" `isInfixOf` (snd x)) attributes)
  rooms <- return $ lookup "Liczba pokoi" attributes >>= parseRooms
  description <- text ("div" @: ["id" @= "textContent"])
  let extras = parseExtras description
  return $
    offer
      { offerDetailed = True,
        offerRentPrice = rent,
        offerArea = area,
        offerOwnerOffer = Just private,
        offerRooms = rooms,
        offerExtras = extras,
        offerDescription = Just description
      }

attrsScraper :: Scraper Text [(Text, Text)]
attrsScraper = chroot ("div" @: [hasClass "descriptioncontent"]) $ do
  offerAttrs <- chroots ("li" @: [hasClass "offer-details__item"]) $ do
    k <- stripSpaces <$> text "span"
    v <- stripSpaces <$> text "strong"
    return (k, v)
  return offerAttrs

offersScraper :: BasicOffer -> Scraper Text [Offer]
offersScraper offer =
  chroots
    (("table" @: ["id" @= "offers_table"]) // "td" @: [hasClass "offer"])
    (offerScraper offer)

olxScraper :: Config Text -> OfferScraper
olxScraper config =
  OfferScraper
    config
    (basicOffer "olx.pl")
    offersScraper
    (Just detailsScraper)
