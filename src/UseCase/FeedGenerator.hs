module UseCase.FeedGenerator
  ( FeedPresenter (..),
    OfferDetailLoader (..),
    showNewSinceLastVisit,
    ResultFeed,
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import UseCase.Offer (DetailedOffer (..), StubOffer)

-- Public interface

newtype ResultFeed = ResultFeed [DetailedOffer] deriving (Show)

class FeedPresenter p where
  present :: p -> ResultFeed -> IO ()

class OfferDetailLoader odl where
  loadDetails :: odl -> StubOffer -> IO DetailedOffer
  loadNewSince :: odl -> UTCTime -> Int -> IO [DetailedOffer]

showNewSinceLastVisit :: (FeedPresenter p, OfferDetailLoader odl) => odl -> p -> Int -> IO ()
showNewSinceLastVisit loader presenter count = do
  lastVisit <- lastVisitTime
  newOffers <- loadNewSince loader lastVisit count
  let feed = ResultFeed newOffers
  present presenter feed

-- Internal

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime = getCurrentTime
