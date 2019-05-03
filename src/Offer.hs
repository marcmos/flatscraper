module Offer ( Offer(..)
             , OfferScraper(..)) where

import Data.Text
import Data.Time (UTCTime)
import Text.HTML.Scalpel

data Offer = Offer
  { offerTitle :: Text
  , offerPriceStr :: Text
  , offerRentPriceStr :: Maybe Text
  , offerURL :: Text
  , offerVisit :: UTCTime
  , offerDetailed :: Bool
  } deriving (Show, Eq)

data OfferScraper = OfferScraper
  { offerListScraper :: UTCTime -> Scraper Text [Offer]
  , offerDetailsScraper :: Maybe (Offer -> Scraper Text Offer) }
