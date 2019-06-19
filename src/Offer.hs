module Offer where

import Data.Text
import Data.Time (UTCTime)
import Text.HTML.Scalpel

data Offer = Offer
  { offerVisit :: UTCTime
  , offerTitle :: Text
  , offerPrice :: Int
  , offerRentPrice :: Maybe Int
  , offerURL :: Text
  , offerRegion :: Maybe Text
  , offerStreet :: Maybe Text
  , offerDirect :: Maybe Bool
  , offerDetailed :: Bool
  , offerScraperName :: Text
  } deriving (Show, Eq)

type BasicOffer = Text -> Int -> Text -> Offer

basicOffer :: Text -> UTCTime -> BasicOffer
basicOffer scraperName timestamp title price url =
  Offer timestamp title price Nothing url Nothing Nothing Nothing False scraperName

data OfferScraper = OfferScraper
  { offerScraperConfig :: Config Text
  , offerTemplate :: UTCTime -> BasicOffer
  , offerListScraper :: BasicOffer -> Scraper Text [Offer]
  , offerDetailsScraper :: Maybe (Offer -> Scraper Text Offer)
  }

scraper
  :: Text
  -> Config Text
  -> (BasicOffer -> Scraper Text [Offer])
  -> Maybe (Offer -> Scraper Text Offer)
  -> OfferScraper
scraper name config =
  OfferScraper config (basicOffer name)

