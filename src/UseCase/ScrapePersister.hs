module UseCase.ScrapePersister
  ( seedOffers,
    storeDetailedOffers,
    OfferStorer (storeOffers),
    scrapeAndStore,
    OfferDetailsLoader (loadDetails),
    NoOpDetailsLoader (NoOpDetailsLoader),
  )
where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Domain.Offer
  ( OfferDetails (_offerBuiltYear),
    OfferView (_offerDetails, _offerURL),
  )
import UseCase.Offer (OfferSeeder (seedOffers))

class OfferStorer os where
  storeOffers :: os -> [OfferView] -> IO ()

class OfferDetailsLoader odl where
  loadDetails :: odl -> OfferView -> IO OfferView

data NoOpDetailsLoader = NoOpDetailsLoader

instance OfferDetailsLoader NoOpDetailsLoader where
  loadDetails _ = return

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
            -- FIXME add a predicate that will tell, if it makes sense to scrape
            -- when details are set by list scraper. For now a simple heuristics
            -- is used that is based on the fact, that all "complete" offers
            -- have room count filled by the list or details scraper.
            Just od ->
              let url = _offerURL ov
                  builtYear = _offerBuiltYear od
               in if "https://www.morizon.pl" `T.isPrefixOf` url
                    && isNothing builtYear
                    then loadDetails detailsScraper ov
                    else return ov
            _ -> loadDetails detailsScraper ov
      )
      detailedOffers
  storeOffers storer detailedOffers'
  return detailedOffers'
