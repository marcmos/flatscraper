module UseCase.DigestGenerator
  ( DigestPresenter (..),
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import UseCase.Offer (OfferView)

-- Public interface

-- newtype ResultFeed = ResultFeed [DetailedOffer] deriving (Show)

class DigestPresenter p where
  present :: p -> [OfferView] -> IO ()

-- showNewSinceLastVisit :: (FeedPresenter p, OfferDetailLoader odl) => odl -> p -> Int -> IO ()
-- showNewSinceLastVisit loader presenter count = do
--   lastVisit <- lastVisitTime
--   newOffers <- loadNewSince loader lastVisit count
--   present presenter newOffers

-- Internal

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime = getCurrentTime
