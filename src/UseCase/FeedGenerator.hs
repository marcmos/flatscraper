module UseCase.FeedGenerator () where

import UseCase.Offer (OfferSeeder (seedOffers))

class FeedPresenter fp where
  present :: fp -> IO ()

presentFeed :: (OfferSeeder os, FeedPresenter fp) => os -> fp -> IO ()
presentFeed seeder presenter = do
  offers <- seedOffers seeder
  undefined