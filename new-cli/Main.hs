{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CaseInsensitive (mk)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T (readFile)
import Network.HTTP.Client (Request, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Persistence.DataAccess (NoOpStorer (NoOpStorer))
import Persistence.SQLite (SQLitePersistence (SQLitePersistence))
import Persistence.ScrapeLoader (ScrapeDetailsLoader (ScrapeDetailsLoader), ScrapeListLoader (ScrapeListLoader))
import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import Scraper.OtodomScraper (detailsScraper, offersScraper)
import Text.HTML.Scalpel (Config (Config), Scraper, scrapeStringLike, utf8Decoder)
import UseCase.Offer (OfferDetailsLoader (loadDetails))
import UseCase.ScrapePersister (scrapeAndStore, seedOffers, storeDetailedOffers)

addLegitHeadersNoScam100 :: Request -> IO Request
addLegitHeadersNoScam100 req =
  return $
    req
      { requestHeaders =
          [ (mk $ encodeUtf8 "Accept", encodeUtf8 "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
            (mk $ encodeUtf8 "Accept-Language", encodeUtf8 "pl,en-US;q=0.7,en;q=0.3"),
            (mk $ encodeUtf8 "Cache-Control", encodeUtf8 "no-cache"),
            (mk $ encodeUtf8 "User-Agent", encodeUtf8 "Mozilla/5.0 (X11; Linux x86_64; rv:67.0) Gecko/20100101 Firefox/67.0")
          ]
      }

scrapeFile :: FilePath -> Scraper Text a -> IO (Maybe a)
scrapeFile path scraper = do
  htmlContent <- T.readFile path
  return $ scrapeStringLike htmlContent scraper

main :: IO ()
main = do
  httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  let config = Config utf8Decoder (Just httpManager)
  let testURL = "https://www.otodom.pl/pl/wyniki/sprzedaz/mieszkanie/cala-polska"
  let testURL2 = "https://www.otodom.pl/pl/wyniki/sprzedaz/mieszkanie/malopolskie/krakow/krakow/krakow?limit=36&ownerTypeSingleSelect=ALL&areaMin=58&areaMax=65&pricePerMeterMax=16000&buildYearMin=2014&floors=%5BFIRST%2CSECOND%2CTHIRD%2CFOURTH%2CFIFTH%2CSIXTH%2CSEVENTH%2CEIGHTH%2CNINTH%2CTENTH%2CABOVE_TENTH%5D&buildingType=%5BBLOCK%2CTENEMENT%2CAPARTMENT%2CLOFT%5D&extras=%5BBALCONY%2CLIFT%2CHAS_PHOTOS%5D&by=LATEST&direction=DESC&viewType=listing"
  -- showNewSinceLastVisit loader cliPresenter 5

  -- scrape from scratch
  -- tests scraper
  -- let loader = ScrapeListLoader config (take 2 <$> offersScraper) testURL2
  let loader = SQLitePersistence
  let detailsLoader = ScrapeDetailsLoader config detailsScraper

  -- offers <- take 2 <$> seedOffers loader
  -- offers <- take 2 . fromJust <$> scrapeFile "testfiles/otodom-list.html" offersScraper
  -- detailedOffers <- mapM (loadDetails detailsLoader) offers

  -- let offerStorer = NoOpStorer
  let offerStorer = SQLitePersistence
  -- detailedOffers <- storeDetailedOffers loader detailsLoader offerStorer
  detailedOffers <- scrapeAndStore loader detailsLoader offerStorer offerStorer

  print
    detailedOffers
  where
    cliPresenter = CLIPresenter 1 2
