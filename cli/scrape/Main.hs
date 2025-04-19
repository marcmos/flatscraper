{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CaseInsensitive (mk)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import DataAccess.ScrapeLoader (ScrapeSource (WebSource), WebScrapers (WebScrapers))
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), Request, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Scraper.OlxScraper
import qualified Scraper.OtodomScraper
import System.Environment (getArgs)
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.ScrapePersister (scrapeAndStore)

addLegitHeadersNoScam100 :: Request -> IO Request
addLegitHeadersNoScam100 req =
  return $
    req
      { requestHeaders =
          [ (mk $ encodeUtf8 "Accept", encodeUtf8 "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
            (mk $ encodeUtf8 "Accept-Language", encodeUtf8 "pl,en-US;q=0.7,en;q=0.3"),
            (mk $ encodeUtf8 "Cache-Control", encodeUtf8 "no-cache"),
            (mk $ encodeUtf8 "User-Agent", encodeUtf8 "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36")
          ]
      }

main :: IO ()
main = do
  -- args <- getArgs
  let testOlxUrl = "https://www.olx.pl/nieruchomosci/mieszkania/sprzedaz/krakow/?search%5Bfilter_float_m%3Afrom%5D=60&search%5Bfilter_float_price%3Ato%5D=1000000&search%5Border%5D=created_at%3Adesc&search%5Bprivate_business%5D=private"
  let args = [testOlxUrl]

  case listToMaybe args of
    Just url -> do
      httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
      let config = Config utf8Decoder (Just httpManager)
      let detailsScrapers = WebScrapers (Just config) [Scraper.OtodomScraper.scraper, Scraper.OlxScraper.scraper]
      let offerSeed = WebSource detailsScrapers url
      let persistence = SQLitePersistence

      _ <- scrapeAndStore offerSeed detailsScrapers persistence persistence
      return ()
    Nothing -> return ()