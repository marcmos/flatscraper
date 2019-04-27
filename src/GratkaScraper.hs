{-# LANGUAGE OverloadedStrings #-}

module GratkaScraper
    ( scrapGratkaOffers
    ) where

import Offer (Offer(..))
import Text.HTML.Scalpel
import Data.Text as T
import Data.List (find)
import Control.Monad (sequence_)

offer :: Scraper Text Offer
offer = do
  name <- stripSpaces . dropShitwords <$> text ("h2" @: [hasClass "teaser__title"])
  price <- stripSpaces <$> text ("p" @: [hasClass "teaser__price"])
  rentPrice <- Data.List.find ("czynsz" `isInfixOf`) <$> texts ("ul" @: [hasClass "teaser__params"] // "li")
  url <- attr "data-href" "article"
  return $ Offer name price rentPrice url

offers :: Scraper Text [Offer]
offers = chroots ("article" @: [hasClass "teaser"]) offer

stripSpaces :: Text -> Text
stripSpaces = T.unwords . T.words

shitwords :: [Text]
shitwords = T.pack <$> ["mieszkanie", "kraków", "wynajmę"]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\x -> T.toLower x `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

scrapGratkaOffers :: String -> IO (Maybe [Offer])
scrapGratkaOffers url = do
  offers <- scrapeURL url offers
  return $ case offers of
    Just offer -> Just offer
    Nothing -> Nothing
