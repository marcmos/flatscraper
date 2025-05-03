{-# LANGUAGE TemplateHaskell #-}

module Domain.Offer where

import Control.Lens (makeLenses)
import Data.Text (Text)

data OfferRef = OfferURLRef
  { _offerRefURL :: Text,
    _offerRefSource :: Maybe Text
  }
  deriving (Show)

data PropertyLocation = PropertyAddress
  { propertyStreet :: Text,
    propertyDistrict :: Maybe Text
  }

data PropertyAttrs = PropertyAttrs
  { propertyRooms :: Maybe Int,
    propertyArea :: Maybe Double
  }

data BuildingAttrs = BuildingAttrs
  { buildingHasElevator :: Maybe Bool,
    buildingFloors :: Maybe Int,
    buildingYearBuilt :: Maybe Int
  }

data OfferView2 = OfferView2
  { _offer2Ref :: OfferRef,
    _offer2LatestPrice :: Int,
    _offer2Title :: Maybe Text,
    _offer2Description :: Maybe Text,
    _offer2Location :: Maybe PropertyLocation,
    _offer2BuildingAttrs :: BuildingAttrs
  }

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
    _offerMunicipalityArea :: Maybe Text,
    _offerHasElevator :: Maybe Bool,
    _offerPropertyFloor :: Maybe Int,
    _offerBuildingFloors :: Maybe Int,
    _offerBuiltYear :: Maybe Int
  }
  deriving (Eq, Show)

makeLenses ''OfferView
makeLenses ''OfferDetails

newOfferView :: Text -> Int -> Double -> Text -> OfferView
newOfferView url price area title = OfferView url price area title Nothing

emptyOffer :: OfferView
emptyOffer = OfferView "" 0 0.0 "" Nothing

emptyDetails :: OfferDetails
emptyDetails = OfferDetails Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pricePerMeter :: OfferView -> Double
pricePerMeter offer = fromIntegral (_offerLatestPrice offer) / _offerArea offer

data ElevatorGuess = BuildingHasManyFloors | BuildingShort | BuildingNewAndHasFloors
  deriving (Show)

data HasElevator = HasElevator
  { _hasElevator :: Bool,
    _hasElevatorGuess :: Maybe ElevatorGuess
  }
  deriving (Show)

hasElevator :: OfferView -> Maybe HasElevator
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerHasElevator = Just True})}) =
  Just $ HasElevator True Nothing
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerHasElevator = Just False})}) =
  Just $ HasElevator False Nothing
hasElevator (OfferView {_offerDetails = Just (OfferDetails {_offerBuildingFloors = Just floors})})
  | floors >= 6 =
      Just $ HasElevator True (Just BuildingHasManyFloors)
  | floors <= 3 =
      Just $ HasElevator False (Just BuildingShort)
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
    | floors >= 5 && bY >= 2000 = Just $ HasElevator True (Just BuildingNewAndHasFloors)
hasElevator _ = Nothing