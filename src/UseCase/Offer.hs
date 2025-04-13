module UseCase.Offer where

import Data.Text (Text)

-- Offer snapshot, seen at some point in time.
-- Minimum viable offer has 3 things: URL, some price and area.
data OfferView = OfferView
  { offerURL :: Text, -- detailed offer URL
    offerLatestPrice :: Int,
    offerArea :: Double,
    offerTitle :: Text,
    offerDetails :: Maybe OfferDetails
  }
  deriving (Eq, Show)

data OfferDetails = OfferDetails
  { offerDescription :: Maybe Text,
    offerRooms :: Maybe Int,
    offerStreet :: Maybe Text,
    offerDistrict :: Maybe Text
  }
  deriving (Eq, Show)

newOfferView :: Text -> Int -> Double -> Text -> OfferView
newOfferView url price area title = OfferView url price area title Nothing

-- class OfferDetailLoader odl where
--   loadDetails :: odl -> StubOffer -> IO DetailedOffer
--   loadNewSince :: odl -> UTCTime -> Int -> IO [OfferView]

class OfferListLoader oll where
  loadOffers :: oll -> IO [OfferView]

class OfferDetailsLoader odl where
  loadDetails :: odl -> OfferView -> IO OfferView
