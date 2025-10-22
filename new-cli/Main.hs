module Main where

import Data.CaseInsensitive (mk)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import DataAccess.SQLite (SQLiteOfferQuery (SQLiteOfferQuery), SQLitePersistence (SQLitePersistence))
import DataAccess.ScrapeLoader (ScrapeSource (FileSource), WebScraper (WebScraper), WebScrapers (WebScrapers))
import Database.Persist.Sqlite (runSqlite)
import Domain.Offer (OfferView (OfferView, _offerInstanceId))
import Network.HTTP.Client (Request, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prefs.Presenter (badgeColorMapper)
import Presenter.HTMLFeedPresenter (v2Presenter)
import qualified Scraper.NieruchOnlineScraper
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Text.Blaze.Html.Renderer.Pretty as H
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.EmergingOffer (EmergingOfferSelector (markAsRssPublished), selectEmergingOfferForRss)
import UseCase.FeedGenerator (showNewSinceLastVisit)
import UseCase.ScrapePersister (OfferStorer (storeOffers), loadDetails, seedOffers)
import View.CLIView (CLIView (CLIView))

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
  let scraper@(WebScraper scraperPack _) = Scraper.NieruchOnlineScraper.scraper
  httpManager <- newManager $ tlsManagerSettings {managerModifyRequest = addLegitHeadersNoScam100}
  let config = Config utf8Decoder (Just httpManager)
  let scrapers = WebScrapers (Just config) [scraper]
  let fs = FileSource scraperPack "testfiles/no-details.html"
  -- let fs = WebSource scrapers "https://krakow.nieruchomosci-online.pl/szukaj.html?3,mieszkanie,sprzedaz,,Krak%C3%B3w,,,,-1000000,6"
  offers <- take 10000 <$> seedOffers fs
  offers' <- mapM (loadDetails scrapers) offers
  -- offers <- take 2 . fromJust <$> scrapeFile "testfiles/otodom-list.html" offersScraper
  print offers'

main :: IO ()
main = do
  let presenter = v2Presenter (Just badgeColorMapper)
  let cliViewer = CLIView (T.pack . H.renderHtml)

  d <- getCurrentTime
  args <- getArgs

  let date =
        T.pack $
          if null args
            then formatTime defaultTimeLocale "%Y-%m-%d" d
            else head args

  let sqliteQuery =
        SQLiteOfferQuery
          SQLitePersistence
          $ "select oi.id, oi.created_at \
            \from scoreboard_boosted sb \
            \left join offer_instance oi on sb.id = oi.id \
            \where date(oi.created_at) = date('"
            <> date
            <> "') \
               \order by sb.score desc"

  showNewSinceLastVisit
    sqliteQuery
    presenter
    cliViewer
    (return Nothing) -- No last visit time
    (\offers -> [("Wszystkie oferty", offers)]) -- Group all offers under a single category
    (const "Flatscraper RSS Feed") -- Title for the feed
