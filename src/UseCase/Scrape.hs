module UseCase.Scrape () where

import UseCase.Offer (DetailedOffer, StubOffer)

class OfferDetailStorer ods where
  storeDetails :: ods -> StubOffer -> IO DetailedOffer
