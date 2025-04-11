module Domain.Offer where

import Data.Text
import Data.Time (UTCTime)
import Text.HTML.Scalpel

-- TODO ilość pokoi klimatyzacja balkon
data OfferExtra = KitchenAnnex | SeparateRooms | Dishwasher | Internet | Oven | Climatronic | Balcony
  deriving (Show, Read, Eq)

data OfferLocation = OfferLocation
  { offerLocationRegion :: Maybe Text,
    offerLocationStreet :: Maybe Text
  }
  deriving (Show, Eq)

data Offer = Offer
  { offerVisit :: UTCTime,
    offerScraperName :: Text,
    offerTitle :: Text,
    offerPrice :: Int,
    offerURL :: Text,
    offerDetailed :: Bool,
    offerDescription :: Maybe Text,
    offerFiltered :: Maybe Text,
    offerArea :: Maybe Int,
    offerRooms :: Maybe Int,
    offerRentPrice :: Maybe Int,
    offerLocation :: Maybe OfferLocation,
    offerOwnerOffer :: Maybe Bool,
    offerExtras :: [OfferExtra]
  }
  deriving (Show, Eq)

type BasicOffer = Text -> Int -> Text -> Offer

basicOffer :: Text -> UTCTime -> BasicOffer
basicOffer scraperName timestamp title price url =
  Offer timestamp scraperName title price url False Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

data OfferScraper = OfferScraper
  { offerScraperConfig :: Config Text,
    offerTemplate :: UTCTime -> BasicOffer,
    offerListScraper :: BasicOffer -> Scraper Text [Offer],
    offerDetailsScraper :: Maybe (Offer -> Scraper Text Offer)
  }
