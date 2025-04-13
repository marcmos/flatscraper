module Persistence.ScrapeLoader
  ( ScrapeListLoader (ScrapeListLoader),
    ScrapeDetailsLoader (ScrapeDetailsLoader),
  )
where

import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Text.HTML.Scalpel (Config, Scraper, URL, scrapeURLWithConfig)
import UseCase.Offer (OfferDetailsLoader (..), OfferListLoader (..), OfferView (offerURL))

data ScrapeListLoader = ScrapeListLoader (Config Text) (Scraper Text [OfferView]) URL

data ScrapeDetailsLoader = ScrapeDetailsLoader (Config Text) (OfferView -> Scraper Text OfferView)

instance OfferDetailsLoader ScrapeDetailsLoader where
  loadDetails (ScrapeDetailsLoader config scraper) offer = do
    putStrLn "scraping details"
    fromJust <$> scrapeURLWithConfig config (Data.Text.unpack $ offerURL offer) (scraper offer)

instance OfferListLoader ScrapeListLoader where
  loadOffers (ScrapeListLoader config scraper url) = do
    fromJust <$> scrapeURLWithConfig config url scraper