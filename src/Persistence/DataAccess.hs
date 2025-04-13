module Persistence.DataAccess (DataAccess (DataAccess), NoOpStorer (NoOpStorer)) where

import UseCase.ScrapePersister (OfferStorer (storeOffers))

data DataAccess = DataAccess

-- instance OfferDetailLoader DataAccess where
--   loadNewSince = undefined

data NoOpStorer = NoOpStorer

instance OfferStorer NoOpStorer where
  storeOffers _ _ = return ()
