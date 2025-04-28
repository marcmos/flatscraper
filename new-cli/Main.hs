{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CaseInsensitive (mk)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, toStrict)
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import DataAccess.ScrapeLoader (ScrapeSource (FileSource, WebSource), WebScraper (WebScraper), WebScrapers (WebScrapers))
import Network.HTTP.Client (Request, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (BadgeColorMapper, cmPricePerMeter),
    HTMLFeedPresenter (HTMLFeedPresenter),
    cmArea,
    defaultColorMapper,
  )
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import qualified Scraper.MorizonScraper
import qualified Scraper.OlxScraper
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.FeedGenerator (FeedViewer (view), present, showNewSinceLastVisit)
import UseCase.ScrapePersister (OfferStorer (storeOffers), loadDetails, seedOffers)
import View.CLIView (CLIView (CLIView))
import View.SMTPView (SMTPView (SMTPView), loadCredentialsFromFile)

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

data NoOpStorer = NoOpStorer

instance OfferStorer NoOpStorer where
  storeOffers _ _ = return ()

testOfflineListScraper :: IO ()
testOfflineListScraper = do
  let scraper@(WebScraper scraperPack _) = Scraper.OlxScraper.scraper
  httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  let config = Config utf8Decoder (Just httpManager)
  let scrapers = WebScrapers (Just config) [scraper]
  let fs = FileSource scraperPack "testfiles/olx-list.html"
  offers <- take 10 <$> seedOffers fs
  offers' <- mapM (loadDetails scrapers) offers
  -- offers <- take 2 . fromJust <$> scrapeFile "testfiles/otodom-list.html" offersScraper
  print offers'

htmlCliView :: CLIView H.Html
htmlCliView = CLIView (TL.toStrict . H.renderHtml)

main :: IO ()
main = do
  -- let testURL = "https://www.otodom.pl/pl/wyniki/sprzedaz/mieszkanie/malopolskie/krakow/krakow/krakow?limit=36&ownerTypeSingleSelect=ALL&areaMin=58&areaMax=65&pricePerMeterMax=16000&buildYearMin=2014&floors=%5BFIRST%2CSECOND%2CTHIRD%2CFOURTH%2CFIFTH%2CSIXTH%2CSEVENTH%2CEIGHTH%2CNINTH%2CTENTH%2CABOVE_TENTH%5D&buildingType=%5BBLOCK%2CTENEMENT%2CAPARTMENT%2CLOFT%5D&extras=%5BBALCONY%2CLIFT%2CHAS_PHOTOS%5D&by=LATEST&direction=DESC&viewType=listing"
  -- let testOfferURL = "https://www.otodom.pl/pl/oferta/2-pokojowe-mieszkanie-38m2-loggia-bezposrednio-ID4umfy"
  -- let testOlxUrl = "https://www.olx.pl/nieruchomosci/mieszkania/sprzedaz/krakow/?search%5Bfilter_float_m%3Afrom%5D=60&search%5Bfilter_float_price%3Ato%5D=1000000&search%5Border%5D=created_at%3Adesc&search%5Bprivate_business%5D=private"
  let sqlite = SQLitePersistence

  -- offers <- take 3 <$> seedOffers sqlite
  -- detailedOffers <- mapM (loadDetails sqlite) offers

  let badgeColorMapper =
        defaultColorMapper
          { cmArea = Just $ \area -> if area >= 70 then "success" else "info",
            cmPricePerMeter = Just $ \ppm -> if ppm <= 12000 then "success" else "info"
          }

  smtpCreds <- loadCredentialsFromFile "smtp-creds"
  case smtpCreds of
    Just creds -> do
      let viewer = SMTPView (TL.toStrict . H.renderHtml) creds
      -- let viewer = htmlCliView
      showNewSinceLastVisit
        sqlite
        (HTMLFeedPresenter (Just badgeColorMapper))
        viewer
    Nothing -> return ()
  where
    -- _ <- emailTest "test"

    -- testOfflineListScraper

    -- showNewSinceLastVisit sqlite cliPresenter

    -- testOfflineListScraper

    -- offers <- seedOffers ss
    -- offers <- scrapeAndStore ss detailsScrapers dbPersistence dbPersistence
    -- print offers

    -- testOfflineListScraper

    -- print detailedOffers

    cliPresenter = CLIPresenter 1 2
