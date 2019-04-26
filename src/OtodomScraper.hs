{-# LANGUAGE OverloadedStrings #-}

module OtodomScraper
    ( scrapOtodomOffers
    ) where

import Offer (Offer(..))
import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad (sequence_)

offer :: Scraper Text Offer
offer = do
  name <- stripSpaces . dropShitwords <$> text ("span" @: [hasClass "offer-item-title"])
  price <- stripSpaces <$> text ("li" @: [hasClass "offer-item-price"])
  url <- attr "href" "a"
  rentPrice <- extractRentPrice <$> attr "href" "a"
  return $ Offer name price rentPrice url


scrapRent :: Scraper Text Text
scrapRent = do
  rentPrice <- Data.List.find ("czynsz" `isInfixOf`) <$> texts ("section" @: [hasClass "section-overview"] // "div" // "ul" // "li")
  case rentPrice of
    Just price -> return $ price
    Nothing -> ""

extractRentPrice :: Text -> Maybe Text
extractRentPrice url = do
  abc <- scrapeURL (unpack url) scrapRent
  abc

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

scrapOtodomOffers :: String -> IO ()
scrapOtodomOffers url = do
  offers <- scrapeURL url offers
  case offers of
    Just offer -> sequence_ $ print <$> offer
    Nothing -> return ()