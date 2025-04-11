{-# LANGUAGE InstanceSigs #-}

module Persistence.DataAccess (DataAccess (DataAccess)) where

import UseCase.FeedGenerator (OfferDetailLoader (loadDetails, loadNewSince))
import UseCase.Offer (DetailedOffer (DetailedOffer), StubOffer (StubOffer))

data DataAccess = DataAccess

instance OfferDetailLoader DataAccess where
  loadDetails :: DataAccess -> StubOffer -> IO DetailedOffer
  loadDetails odl stubOffer = undefined

  loadNewSince = undefined