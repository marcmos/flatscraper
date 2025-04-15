module UseCase.FeedGenerator (FeedPresenter (present), presentFeed) where

import UseCase.Offer (OfferSeeder (seedOffers), OfferView)

class FeedPresenter fp where
  present :: fp -> [OfferView] -> IO ()

presentFeed :: (OfferSeeder os, FeedPresenter fp) => os -> fp -> IO ()
presentFeed seeder presenter = do
  offers <- seedOffers seeder
  present presenter offers