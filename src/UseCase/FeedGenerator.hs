module UseCase.FeedGenerator (FeedPresenter, present, getFreshAndPresent) where

import UseCase.Offer (DetailedOffer(..), StubOffer)

newtype ResultFeed = ResultFeed [DetailedOffer] deriving (Show)

class FeedPresenter p where
    present :: p -> ResultFeed -> IO ()

class OfferDetailLoader odl where
    loadDetails :: odl -> StubOffer -> IO DetailedOffer

getFreshAndPresent :: FeedPresenter p => p -> IO ()
getFreshAndPresent presenter = do
    present presenter mockData
    where mockData = ResultFeed [DetailedOffer 1 "test offer"]



showNewSinceLastVisit :: FeedPresenter p => p -> Int -> Int -> IO ()
showNewSinceLastVisit presenter count lastVisitTimestamp = undefined