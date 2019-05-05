module Offer ( Offer(..)
             , OfferScraper(..)) where

import Data.Text
import Data.Time (UTCTime)
import Text.HTML.Scalpel

data Offer = Offer
  { offerTitle :: Text
  , offerPrice :: Int
  , offerRentPrice :: Maybe Int
  , offerURL :: Text
  , offerVisit :: UTCTime
  , offerDetailed :: Bool
  , offerScraperName :: Text
  } deriving (Show, Eq)

data OfferScraper = OfferScraper
  { offerListScraper :: UTCTime -> Scraper Text [Offer]
  , offerDetailsScraper :: Maybe (Offer -> Scraper Text Offer) }
