{-# LANGUAGE TemplateHaskell #-}

module Domain.Offer where

import Control.Lens (makeLenses)
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
    _offerDistrict :: Maybe Text,
    _offerHasElevator :: Maybe Bool,
    _offerApartmentFloor :: Maybe Int,
    _offerBuildingFloors :: Maybe Int,
    _offerBuiltYear :: Maybe Int
  }
  deriving (Eq, Show)

makeLenses ''OfferView
makeLenses ''OfferDetails

newOfferView :: Text -> Int -> Double -> Text -> OfferView
newOfferView url price area title = OfferView url price area title Nothing

emptyDetails :: OfferDetails
emptyDetails = OfferDetails Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pricePerMeter :: OfferView -> Double
pricePerMeter offer = fromIntegral (_offerLatestPrice offer) / _offerArea offer

data HasElevator = HasElevator
  { _hasElevator :: Bool,
    _hasElevatorGuess :: Bool
  }

hasElevator :: OfferView -> Maybe HasElevator
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerHasElevator = Just True})}) =
  Just $ HasElevator True False
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerHasElevator = Just False})}) =
  Just $ HasElevator False False
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerBuildingFloors = Just floors})})
  | floors >= 6 =
      Just $ HasElevator True True
  | floors <= 3 =
      Just $ HasElevator False True
hasElevator
  ( OfferView
      { _offerDetails =
          Just
            ( OfferDetails
                { _offerBuildingFloors = Just floors,
                  _offerBuiltYear = Just bY
                }
              )
      }
    )
    | floors >= 4 && bY >= 2000 = Just $ HasElevator True True
hasElevator _ = Nothing