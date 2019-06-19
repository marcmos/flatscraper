module Offer where

import Data.Text
import Data.Time (UTCTime)
import Text.HTML.Scalpel

data Offer = Offer
  { offerTitle :: Text
  , offerPrice :: Int
  , offerRentPrice :: Maybe Int
  , offerURL :: Text
  , offerVisit :: UTCTime
  , offerRegion :: Maybe Text
  , offerStreet :: Maybe Text
  , offerDirect :: Maybe Bool
  , offerDetailed :: Bool
  , offerScraperName :: Text
  } deriving (Show, Eq)

basicOffer :: Text -> Int -> Text -> UTCTime -> Offer
basicOffer scraperName title price url timestamp =
  Offer title price Nothing url timestamp Nothing Nothing Nothing False scraperName

data OfferScraper = OfferScraper
  { offerScraperConfig :: Config Text
  , offerTemplate :: Text -> Int -> Text -> UTCTime -> Offer
  , offerListScraper :: UTCTime -> Scraper Text [Offer]
  , offerDetailsScraper :: Maybe (Offer -> Scraper Text Offer)
  }

offerScraper
  :: Text
  -> Config Text
  -> (UTCTime -> Scraper Text [Offer])
  -> Maybe (Offer -> Scraper Text Offer)
offerScraper name config listScraper detailsScraper =
  OfferScraper config (basicOffer name) listScraper offerScraper

