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
import qualified Scraper.NieruchOnlineScraper
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
  let morizonUrl = "https://www.morizon.pl/mieszkania/najnowsze/krakow/?ps%5Bliving_area_from%5D=60&ps%5Blocation%5D%5Bmap%5D=1&ps%5Blocation%5D%5Bmap_bounds%5D=50.435095053953,20.273921561471:49.655401073385,19.735824538529&ps%5Bowner%5D%5B0%5D=3&ps%5Bowner%5D%5B1%5D=1&ps%5Bprice_to%5D=1000000&ps%5Bwith_price%5D=1"
  let otodomUrl = "https://www.otodom.pl/pl/wyniki/sprzedaz/mieszkanie/wiele-lokalizacji?limit=36&ownerTypeSingleSelect=ALL&areaMin=60&priceMax=1000000&pricePerMeterMax=16000&buildYearMax=2025&locations=%5Bmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze%2Fplaszow%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fmistrzejowice%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fstare-miasto%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fnowa-huta%2Fnowa-huta%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpradnik-bialy%2Fkrowodrza-gorka%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fkrowodrza%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze%2Fkabel%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze%2Fstare-podgorze%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze%2Frybitwy%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze-duchackie%2Fwola-duchacka%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fbienczyce%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fczyzyny%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpradnik-bialy%2Fazory%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpradnik-bialy%2Fpradnik-bialy%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpradnik-bialy%2Fzabiniec%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fbronowice%2Fbronowice%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fzwierzyniec%2Fblonia%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Flagiewniki--borek-falecki%2Fcegielniana%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fbronowice%2Fbronowice-male%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fgrzegorzki%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fdebniki%2Fludwinow%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Flagiewniki--borek-falecki%2Flagiewniki%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fzwierzyniec%2Fzwierzyniec%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpradnik-czerwony%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fdebniki%2Fdebniki%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fpodgorze%2Fzablocie%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Flagiewniki--borek-falecki%2Fborek-falecki%2Cmalopolskie%2Fkrakow%2Fkrakow%2Fkrakow%2Fdebniki%2Fruczaj%5D&by=LATEST&direction=DESC&viewType=listing"
  let nieruchOnlineUrl = "https://krakow.nieruchomosci-online.pl/szukaj.html?3,mieszkanie,sprzedaz,,Krak%C3%B3w,,,,-1000000,60"
  let args =
        if null urls
          then
            [ morizonUrl,
              olxUrl,
              otodomUrl,
              nieruchOnlineUrl
            ]
          else urls

  httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  let config = Config utf8Decoder (Just httpManager)
  let detailsScrapers =
        WebScrapers
          (Just config)
          [ Scraper.MorizonScraper.scraper,
            Scraper.OtodomScraper.scraper,
            Scraper.OlxScraper.scraper,
            Scraper.NieruchOnlineScraper.scraper
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
