module Main where

import Control.Monad (forM_)
import Data.CaseInsensitive (mk)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import DataAccess.ScrapeLoader (ScrapeSource (WebSource), WebScrapers (WebScrapers))
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), Request, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Scraper.MorizonScraper
import qualified Scraper.OlxScraper
import qualified Scraper.OtodomScraper
import System.Environment (getArgs)
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.ScrapePersister (NoOpDetailsLoader (NoOpDetailsLoader), OfferDetailsLoader, scrapeAndStore)

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
  args <- getArgs

  let persistence = SQLitePersistence
  let (urls, rescrape) = case args of
        "rescrape" : xs -> (xs, True)
        xs -> (xs, False)

  let olxUrl = "https://www.olx.pl/nieruchomosci/mieszkania/sprzedaz/krakow/?search%5Border%5D=created_at:desc&search%5Bfilter_float_price:to%5D=1000000&search%5Bfilter_float_m:from%5D=60"
  let morizonUrl = "https://www.morizon.pl/mieszkania/najnowsze/krakow/?ps%5Bliving_area_from%5D=60&ps%5Blocation%5D%5Bmap%5D=1&ps%5Blocation%5D%5Bmap_bounds%5D=50.212671292491,20.186952058559:49.880416226457,19.822794041442&ps%5Bowner%5D%5B0%5D=3&ps%5Bowner%5D%5B1%5D=1&ps%5Bowner%5D%5B2%5D=2&ps%5Bprice_to%5D=1000000&ps%5Bwith_price%5D=1"
  let args =
        if null urls
          then
            [ morizonUrl,
              olxUrl
            ]
          else urls

  httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  let config = Config utf8Decoder (Just httpManager)
  let detailsScrapers =
        WebScrapers
          (Just config)
          [ Scraper.MorizonScraper.scraper,
            Scraper.OtodomScraper.scraper,
            Scraper.OlxScraper.scraper
          ]

  forM_
    args
    ( \x ->
        let offerSeed = WebSource detailsScrapers x
         in if rescrape
              then
                scrapeAndStore
                  offerSeed
                  detailsScrapers
                  NoOpDetailsLoader
                  persistence
              else
                scrapeAndStore
                  offerSeed
                  detailsScrapers
                  persistence
                  persistence
    )
