module Offer
  ( Offer(..)
  ) where

import Data.Text
import Data.Time.LocalTime

data Offer = Offer { offerTitle :: Text
                   , offerPriceStr :: Text
                   , offerRentPriceStr :: Maybe Text
                   , offerURL :: Text
                   , offerVisit :: Maybe LocalTime
                   } deriving (Show, Eq)
