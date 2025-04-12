{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CaseInsensitive (mk)
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T (readFile)
import Network.HTTP.Client (Request, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Persistence.BasicScraperLoader (BasicScraperLoader (BasicScraperLoader))
import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import Scraper.OtodomScraper (offersScraper)
import Text.HTML.Scalpel (Config (Config), scrapeStringLike, utf8Decoder)
import UseCase.Offer (OfferDetailsLoader (loadDetails))
import UseCase.ScrapePersister (loadOffers)

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

scrapeFile path scraper = do
  htmlContent <- T.readFile path
  return $ scrapeStringLike htmlContent scraper

main :: IO ()
main =
  -- httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  -- let config = Config utf8Decoder (Just httpManager)
  -- let testURL = "https://www.otodom.pl/pl/wyniki/sprzedaz/mieszkanie/cala-polska"

  -- scrape from scratch
  -- tests scraper
  -- let loader = BasicScraperLoader config Scraper.OtodomScraper.offersScraper testURL
  -- offers <- loadOffers loader
  -- -- detailedOffers <- mapM (loadDetails loader) offers
  -- print offers

  do
    offers <- fromJust <$> scrapeFile "testfiles/otodom-list.html" offersScraper
    print offers

    -- wczytac list loader scrapera
    -- wczytac details loader z bazy
    -- wczytac details loader ze scrapera details
    -- zawolac persist

    -- showNewSinceLastVisit loader cliPresenter 5
    return ()
  where
    cliPresenter = CLIPresenter 1 2
