module UseCase.Offer where

import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Offer (OfferView)
import Domain.PublicTransport (TripSummary)

class OfferSeeder os where
  seedOffers :: os -> IO [OfferView]

class QueryAccess a where
  getOffersCreatedAfter :: a -> UTCTime -> IO [OfferView]
  fetchTripSummary :: a -> Text -> IO (Maybe TripSummary)
