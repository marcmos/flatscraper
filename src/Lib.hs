{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Text.HTML.Scalpel
import Data.Text as T
import Data.Text.IO as T
import Control.Monad (sequence_)

data Offer = Offer Text Text deriving (Show, Eq)

offer :: Scraper Text Offer
offer = do
  name <- text $ "h2" @: [hasClass "teaser__title"]
  price <- text $ "p" @: [hasClass "teaser__price"]
  return $ Offer name price

offers :: Scraper Text [Offer]
offers = chroots ("article" @: [hasClass "teaser"]) offer

stripSpaces = T.unwords . T.words

shitwords = T.pack <$> ["mieszkanie", "kraków", "wynajmę"]

dropWords keywords x = T.unwords $ Prelude.filter (\x -> T.toLower x `notElem` keywords) $ T.words x

dropShitwords = dropWords shitwords

someFunc :: IO ()
someFunc = do
  offers <- scrapeURL "https://gratka.pl/nieruchomosci/mieszkania/krakow/krowodrza/wynajem?liczba-pokoi:min=3&liczba-pokoi:max=5&cena-calkowita:max=3000&sort=newest" offers
  case offers of
    Just offer -> sequence_ $ T.putStrLn . T.strip . dropShitwords . (\(Offer x y) -> stripSpaces x <> " " <> stripSpaces y) <$> offer
    Nothing -> return ()
