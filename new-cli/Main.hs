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
import Database.Persist (PersistValue)
import Database.Persist.Sqlite (runSqlite)
import Domain.Offer (OfferView (OfferView, _offerInstanceId))
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
import System.IO (hPutStrLn, stderr)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import Text.HTML.Scalpel (Config (Config), utf8Decoder)
import UseCase.EmergingOffer (EmergingOfferSelector (markAsRssPublished), selectEmergingOfferForRss)
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

main :: IO ()
main = do
  let rssPresenter = RSSFeedPresenter
  let cliViewer = CLIView id

  let tramQuery =
        "select id \
        \from score_offer_tram_v1 \
        \natural join offer_instance \
        \where date(created_at) >= date('now', '-1 day') \
        \order by total_score desc"
  let combinedQuery =
        "select id \
        \from score_offer_combined_v1 \
        \natural join offer_instance \
        \where date(created_at) >= date('now', '-1 day') \
        \order by total_score desc"
  let joinedQuery =
        "select id \
        \from scoreboard_boosted \
        \natural join offer_instance \
        \where date(created_at) >= date('now', '-1 day') \
        \order by score desc"

  args <- getArgs

  let (feedId, queries) = case args of
        ["v1"] -> do
          ("v1", [(tramQuery, []), (combinedQuery, [])])
        ["v2"] -> do
          ("v2", [(joinedQuery, [])])
        _ -> do
          ("none", []) :: (Text, [(Text, [PersistValue])])

  o <- runSqlite "flatscraper.sqlite" $ selectEmergingOfferForRss feedId queries

  now <- getCurrentTime
  case o of
    Nothing -> hPutStrLn stderr "No offers found."
    Just (OfferView {_offerInstanceId = offerId}) -> do
      case offerId of
        Nothing -> hPutStrLn stderr "Offer ID is missing."
        Just offerId' ->
          runSqlite "flatscraper.sqlite" $
            markAsRssPublished offerId' now "scoreboard_boosted" feedId

  let sqliteQuery =
        SQLiteOfferQuery
          SQLitePersistence
          $ "select offer_id, score \
            \from published_offer \
            \where feed_id = '"
            <> feedId
            <> "'\
               \order by id desc \
               \limit 10"

  showNewSinceLastVisit
    sqliteQuery
    rssPresenter
    cliViewer
    (return Nothing) -- No last visit time
    (\offers -> [("Offers", offers)]) -- Group all offers under a single category
    (const "Flatscraper RSS Feed") -- Title for the feed
