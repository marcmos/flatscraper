{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Offer where

import Control.Lens (makeLenses)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (empty, insert, lookup)
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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

newtype OfferInstanceId = OfferInstanceId {unOfferInstanceId :: Int64}
  deriving (Show, Eq, Ord)

-- Offer snapshot, seen at some point in time.
-- Minimum viable offer has 3 things: URL, some price and area.
data OfferView = OfferView
  { _offerInstanceId :: Maybe OfferInstanceId,
    _offerURL :: Text, -- detailed offer URL
    _offerLatestPrice :: Int,
    _offerArea :: Double,
    _offerTitle :: Text,
    _offerDetails :: Maybe OfferDetails,
    _offerCreatedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

data OfferCoordinates = OfferExactCoordinates
  { _latitude :: Double,
    _longitude :: Double
  }
  deriving (Eq, Show)

data OfferMarket = MarketPrimary | MarketSecondary
  deriving (Eq, Show)

data BoolAttr = AirConditioning | Balcony
  deriving (Show, Eq, Generic)

instance Hashable BoolAttr

type OfferBoolAttrs = HashMap BoolAttr Bool

emptyBoolAttrs :: OfferBoolAttrs
emptyBoolAttrs = HashMap.empty

data OfferDetails = OfferDetails
  { _offerDescription :: Maybe Text,
    _offerRooms :: Maybe Int,
    _offerStreet :: Maybe Text,
    _offerDistrict :: Maybe Text,
    _offerMunicipalityArea :: Maybe Text,
    _offerHasElevator :: Maybe Bool,
    _offerPropertyFloor :: Maybe Int,
    _offerBuildingFloors :: Maybe Int,
    _offerBuiltYear :: Maybe Int,
    _offerCoordinates :: Maybe OfferCoordinates,
    _offerMarket :: Maybe OfferMarket,
    _offerBoolAttrs :: OfferBoolAttrs
  }
  deriving (Eq, Show)

makeLenses ''OfferView
makeLenses ''OfferDetails

newOfferView :: Text -> Int -> Double -> Text -> OfferView
newOfferView url price area title =
  OfferView
    Nothing
    url
    price
    area
    title
    Nothing
    Nothing

emptyOffer :: OfferView
emptyOffer = OfferView Nothing "" 0 0.0 "" Nothing Nothing

emptyDetails :: OfferDetails
emptyDetails =
  OfferDetails
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    emptyBoolAttrs

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

hasBoolAttr :: BoolAttr -> OfferView -> Maybe Bool
hasBoolAttr attr offer =
  let attrs = _offerBoolAttrs <$> _offerDetails offer
   in attrs >>= hasBoolAttrInAttrs attr

-- | Check if a BoolAttr is present in OfferBoolAttrs
hasBoolAttrInAttrs :: BoolAttr -> OfferBoolAttrs -> Maybe Bool
hasBoolAttrInAttrs = HashMap.lookup

updateBoolAttr :: OfferView -> BoolAttr -> Bool -> OfferView
updateBoolAttr ov attr v =
  let details = fromMaybe emptyDetails (_offerDetails ov)
   in ov
        { _offerDetails =
            Just
              details
                { _offerBoolAttrs =
                    HashMap.insert attr v (_offerBoolAttrs details)
                }
        }