module UseCase.Offer where

import Data.Text (Text)
import Data.Time (UTCTime)

-- Offer snapshot, seen at some point in time.
-- Minimum viable offer has 3 things: URL, some price and area.
data OfferView = OfferView
  { offerURL :: Text, -- detailed offer URL
    offerLatestPrice :: Float,
    offerArea :: Float,
    offerDescription :: Maybe Text,
    offerDetails :: Maybe OfferDetails
  }
  deriving (Eq, Show)

data OfferDetails = OfferDetails
  { offerTitle :: Maybe Text,
    offerRooms :: Maybe Int
  }
  deriving (Eq, Show)

newOfferView :: Text -> Float -> Float -> OfferView
newOfferView url price area = OfferView url price area Nothing Nothing

-- class OfferDetailLoader odl where
--   --  loadDetails :: odl -> StubOffer -> IO DetailedOffer
--   loadNewSince :: odl -> UTCTime -> Int -> IO [OfferView]

class OfferListLoader oll where
  loadOffers :: oll -> IO [OfferView]

class OfferDetailsLoader odl where
  loadDetails :: odl -> OfferView -> IO OfferView
