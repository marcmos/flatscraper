module UseCase.ScrapePersister
  ( loadOffers,
    storeDetailedOffers,
    OfferStorer (storeOffers),
    scrapeAndStore,
  )
where

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
  IO [OfferView]
storeDetailedOffers listLoader detailLoader storer = do
  offers <- loadOffers listLoader
  detailedOffers <- mapM (loadDetails detailLoader) offers
  storeOffers storer detailedOffers
  return detailedOffers

-- wczytac list loader scrapera
-- wczytac details loader z bazy
-- wczytac details loader ze scrapera details
-- zawolac persist
scrapeAndStore ::
  (OfferListLoader oll, OfferDetailsLoader odl1, OfferDetailsLoader odl2, OfferStorer os) =>
  oll ->
  odl1 ->
  odl2 ->
  os ->
  IO [OfferView]
scrapeAndStore scraper detailsScraper detailsLoader storer = do
  offers <- loadOffers scraper
  detailedOffers <- mapM (loadDetails detailsLoader) offers
  detailedOffers' <-
    mapM
      ( \ov -> do
          case offerDetails ov of
            Just _ -> return ov
            Nothing -> loadDetails detailsScraper ov
      )
      detailedOffers
  storeOffers storer detailedOffers'
  return detailedOffers'
