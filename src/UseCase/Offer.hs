module UseCase.Offer where

import Data.Text (Text)

data OfferView = OfferView
  { offerURL :: Text,
    offerLatestPrice :: Float,
    offerArea :: Float,
    offerDescription :: Maybe Text
  }

data StubOffer = StubOffer Int String deriving (Show)

data DetailedOffer = DetailedOffer Int String deriving (Show)
