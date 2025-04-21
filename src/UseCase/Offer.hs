module UseCase.Offer where

import Data.Time (UTCTime)
import Domain.Offer (OfferView)

class OfferSeeder os where
  seedOffers :: os -> IO [OfferView]

class QueryAccess a where
  getOffersCreatedAfter :: a -> UTCTime -> IO [OfferView]
