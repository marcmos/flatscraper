module Persistence.BasicScraperLoader (BasicScraperLoader (BasicScraperLoader)) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Text.HTML.Scalpel (Config, Scraper, URL, scrapeURLWithConfig)
import UseCase.Offer (OfferDetailsLoader (..), OfferListLoader (..), OfferView)

data BasicScraperLoader = BasicScraperLoader (Config Text) (Scraper Text [OfferView]) URL

-- instance OfferDetailLoader BasicScraperLoader where
--   loadNewSince (BasicScraperLoader config scraper url) _ limit = do
--     take limit . fromJust <$> scrapeURLWithConfig config url scraper

instance OfferDetailsLoader BasicScraperLoader where
  loadDetails _ = return

instance OfferListLoader BasicScraperLoader where
  loadOffers (BasicScraperLoader config scraper url) = do
    fromJust <$> scrapeURLWithConfig config url scraper