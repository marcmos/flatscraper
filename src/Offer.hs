module Offer
  ( Offer(..)
  ) where

import Data.Text
import Data.Time (UTCTime)

data Offer = Offer { offerTitle :: Text
                   , offerPriceStr :: Text
                   , offerRentPriceStr :: Maybe Text
                   , offerURL :: Text
                   , offerVisit :: Maybe UTCTime
                   } deriving (Show, Eq)
