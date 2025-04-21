module UseCase.DigestGenerator
  ( DigestPresenter (..),
    OfferDigest (..),
    showNewSinceLastVisit,
    OfferDigestItem (_itemURL),
  )
where

import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Domain.Offer (OfferView (OfferView, _offerURL))
import UseCase.Offer (QueryAccess (getOffersCreatedAfter))

-- Public interface

-- newtype ResultFeed = ResultFeed [DetailedOffer] deriving (Show)

data OfferDigestItem = OfferDigestItem
  { _itemURL :: Text
  }

newtype OfferDigest = OfferDigest
  { offersAdded :: [OfferDigestItem]
  }

class DigestPresenter p where
  present :: p -> OfferDigest -> IO ()

showNewSinceLastVisit :: (DigestPresenter p, QueryAccess a) => a -> p -> IO ()
showNewSinceLastVisit queryAccess presenter = do
  lastVisit <- lastVisitTime
  newOffers <- getOffersCreatedAfter queryAccess lastVisit
  present presenter (OfferDigest $ map repack newOffers)
  where
    repack (OfferView {_offerURL = url}) =
      OfferDigestItem {_itemURL = url}

-- Internal

-- FIXME: mock
lastVisitTime :: IO UTCTime
lastVisitTime = do
  addUTCTime (-(24 * 3600)) <$> getCurrentTime
