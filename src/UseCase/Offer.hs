{-# LANGUAGE TemplateHaskell #-}

module UseCase.Offer where

import Control.Lens
import Data.Text (Text)

-- Offer snapshot, seen at some point in time.
-- Minimum viable offer has 3 things: URL, some price and area.
data OfferView = OfferView
  { _offerURL :: Text, -- detailed offer URL
    _offerLatestPrice :: Int,
    _offerArea :: Double,
    _offerTitle :: Text,
    _offerDetails :: Maybe OfferDetails
  }
  deriving (Eq, Show)

data OfferDetails = OfferDetails
  { _offerDescription :: Maybe Text,
    _offerRooms :: Maybe Int,
    _offerStreet :: Maybe Text,
    _offerDistrict :: Maybe Text
  }
  deriving (Eq, Show)

emptyDetails :: OfferDetails
emptyDetails =
  OfferDetails
    { _offerDescription = Nothing,
      _offerRooms = Nothing,
      _offerStreet = Nothing,
      _offerDistrict = Nothing
    }

makeLenses ''OfferView
makeLenses ''OfferDetails

newOfferView :: Text -> Int -> Double -> Text -> OfferView
newOfferView url price area title = OfferView url price area title Nothing

class OfferSeeder os where
  seedOffers :: os -> IO [OfferView]

class OfferDetailsLoader odl where
  loadDetails :: odl -> OfferView -> IO OfferView
