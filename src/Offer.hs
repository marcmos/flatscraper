module Offer (Offer (Offer)) where

import Data.Text as T

data Offer = Offer { title :: Text
                   , priceStr :: Text
                   , rentPriceStr :: Maybe Text
                   , url :: Text
                   } deriving (Show, Eq)
