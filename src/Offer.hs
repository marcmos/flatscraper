module Offer
  ( Offer(..)
  ) where

import Data.Text

data Offer = Offer { offerTitle :: Text
                   , offerPriceStr :: Text
                   , offerRentPriceStr :: Maybe Text
                   , offerURL :: Text
                   } deriving (Show, Eq)
