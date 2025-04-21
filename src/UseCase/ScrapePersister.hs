module UseCase.ScrapePersister
  ( seedOffers,
    storeDetailedOffers,
    OfferStorer (storeOffers),
    scrapeAndStore,
    OfferDetailsLoader (loadDetails),
  )
where

import Domain.Offer (OfferView (_offerDetails))
import UseCase.Offer (OfferSeeder (seedOffers))

class OfferStorer os where
  storeOffers :: os -> [OfferView] -> IO ()

class OfferDetailsLoader odl where
  loadDetails :: odl -> OfferView -> IO OfferView

storeDetailedOffers ::
  (OfferSeeder oll, OfferDetailsLoader odl, OfferStorer os) =>
  oll ->
  odl ->
  os ->
  IO [OfferView]
storeDetailedOffers listLoader detailLoader storer = do
  offers <- seedOffers listLoader
  detailedOffers <- mapM (loadDetails detailLoader) offers
  storeOffers storer detailedOffers
  return detailedOffers

-- wczytac list loader scrapera
-- wczytac details loader z bazy
-- wczytac details loader ze scrapera details
-- zawolac persist
scrapeAndStore ::
  (OfferSeeder oll, OfferDetailsLoader odl1, OfferDetailsLoader odl2, OfferStorer os) =>
  oll ->
  odl1 ->
  odl2 ->
  os ->
  IO [OfferView]
scrapeAndStore scraper detailsScraper detailsLoader storer = do
  offers <- seedOffers scraper
  detailedOffers <- mapM (loadDetails detailsLoader) offers
  detailedOffers' <-
    mapM
      ( \ov -> do
          case _offerDetails ov of
            Just _ -> return ov
            Nothing -> loadDetails detailsScraper ov
      )
      detailedOffers
  storeOffers storer detailedOffers'
  return detailedOffers'
