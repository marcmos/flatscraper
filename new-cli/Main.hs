module Main where

import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, toStrict)
import Data.Time (getCurrentTime)
import DataAccess.Mobroute (MobrouteProvider (MobrouteProvider))
import DataAccess.SQLite (SQLiteOfferQuery (SQLiteOfferQuery), SQLitePersistence (SQLitePersistence))
import DataAccess.ScrapeLoader (ScrapeSource (FileSource, WebSource), WebScraper (WebScraper), WebScrapers (WebScrapers))
import Network.HTTP.Client (Request, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import Presenter.HTMLFeedPresenter (HTMLFeedPresenter (HTMLFeedPresenter))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import qualified Scraper.MorizonScraper
import qualified Scraper.NieruchOnlineScraper
import qualified Scraper.OlxScraper
import qualified Scraper.OtodomScraper
import System.Environment (getArgs)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.FeedGenerator (FeedPresenter (present), Formatters, OfferFeed (OfferFeed), OfferFeedItem, showNewSinceLastVisit)
import UseCase.Offer (QueryAccess (getOffersCreatedAfter))
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

query :: Text -> Text
query profile =
  "select id, score from scoreboard_"
    <> profile
    <> " order by score desc \
       \limit 6"

main :: IO ()
main = do
  let rssPresenter = RSSFeedPresenter
  let cliViewer = CLIView id

  args <- getArgs
  let scoreboardTable = maybe "tram" T.pack (listToMaybe args)
  let scoreboardQuery = query scoreboardTable

  let sqliteQuery = SQLiteOfferQuery SQLitePersistence scoreboardQuery

  showNewSinceLastVisit
    sqliteQuery
    rssPresenter
    cliViewer
    (return Nothing) -- No last visit time
    (\offers -> [("Offers", offers)]) -- Group all offers under a single category
    (\_ -> "Flatscraper RSS Feed") -- Title for the feed
