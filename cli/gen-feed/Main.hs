module Main where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import DataAccess.SQLite (SQLiteOfferQuery (SQLiteOfferQuery), SQLitePersistence (SQLitePersistence))
import Database.Persist (PersistValue)
import Database.Persist.Sqlite (runSqlite)
import Domain.Offer (OfferView (OfferView, _offerInstanceId))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import UseCase.EmergingOffer (EmergingOfferSelector (markAsRssPublished), selectEmergingOfferForRss)
import UseCase.FeedGenerator (showNewSinceLastVisit)
import View.CLIView (CLIView (CLIView))

query :: Text -> Text
query profile =
  "select id, score, created_at from scoreboard_"
    <> profile
    <> " order by score desc \
       \limit 6"

v1 :: [String] -> IO ()
v1 args = do
  let rssPresenter = RSSFeedPresenter
  let cliViewer = CLIView id

  let scoreboardTable = maybe "tram" T.pack (listToMaybe args)
  let scoreboardQuery = query scoreboardTable

  let sqliteQuery = SQLiteOfferQuery SQLitePersistence scoreboardQuery

  showNewSinceLastVisit
    sqliteQuery
    rssPresenter
    cliViewer
    (return Nothing) -- No last visit time
    (\offers -> [("Offers", offers)]) -- Group all offers under a single category
    (const "Flatscraper RSS Feed") -- Title for the feed

v2 :: [String] -> IO ()
v2 args = do
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
          $ "select offer_id, score, created_at \
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    "v1" : rest -> v1 rest
    "v2" : rest -> v2 rest
    _ -> v1 ["tram"]
