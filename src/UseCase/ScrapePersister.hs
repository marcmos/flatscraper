module UseCase.ScrapePersister (loadOffers) where

import UseCase.Offer
  ( OfferDetailsLoader (loadDetails),
    OfferListLoader (loadOffers),
    OfferView,
    offerDetails,
  )

class OfferStorer os where
  storeOffers :: os -> [OfferView] -> IO ()

storeDetailedOffers ::
  (OfferListLoader oll, OfferDetailsLoader odl, OfferStorer os) =>
  oll ->
  odl ->
  os ->
  IO ()
storeDetailedOffers listLoader detailLoader storer = do
  offers <- loadOffers listLoader
  detailedOffers <- mapM (loadDetails detailLoader) offers
  storeOffers storer detailedOffers

scrapeDetails :: (OfferStorer os) => os -> [OfferView] -> IO ()
scrapeDetails os ov = do
  storeOffers os ov
