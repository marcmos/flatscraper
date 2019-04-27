{-# LANGUAGE OverloadedStrings #-}

module OtodomScraper
    ( scrapOtodomOffers
    , flatScrapOtodomOffers
    ) where

import Offer (Offer(..))
import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad (sequence_, (>=>))
import Data.Maybe (Maybe(Just))

offer :: Scraper Text Offer
offer = do
  name <- stripSpaces . dropShitwords <$> text ("span" @: [hasClass "offer-item-title"])
  price <- stripSpaces <$> text ("li" @: [hasClass "offer-item-price"])
  url <- attr "href" "a"
  return $ Offer name price Nothing url

scrapRent :: Scraper Text [Text]
scrapRent = texts ("section" @: [hasClass "section-overview"] // "div" // "ul" // "li")

extractRentPrice :: Text -> IO (Maybe Text)
extractRentPrice url = do
  scraped <- scrapeURL (unpack url) scrapRent
  return $ scraped >>= Data.List.find ("Czynsz" `isInfixOf`)

stripSpaces :: Text -> Text
stripSpaces = T.unwords . T.words

shitwords :: [Text]
shitwords = T.pack <$> ["mieszkanie", "kraków", "wynajmę"]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\x -> T.toLower x `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

offers :: Scraper Text [Offer]
offers = chroots ("article" @: [hasClass "offer-item"]) offer

augmentByOfferRentPrice :: Offer -> IO Offer
augmentByOfferRentPrice offer = do
  price <- extractRentPrice $ Offer.url offer
  return $ case price of
    Just price -> offer {rentPriceStr = Just price}
    Nothing    -> offer

flatScrapOtodomOffers :: String -> IO (Maybe [Offer])
flatScrapOtodomOffers url = scrapeURL url offers

scrapOtodomOffers :: String -> IO (Maybe [Offer])
scrapOtodomOffers url = do
  x <- flatScrapOtodomOffers url
  case x of
    Just o -> Just <$> mapM augmentByOfferRentPrice o
    Nothing -> return Nothing
