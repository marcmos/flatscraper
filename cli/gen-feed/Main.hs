module Main where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import DataAccess.SQLite (SQLiteOfferQuery (SQLiteOfferQuery), SQLitePersistence (SQLitePersistence))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import System.Environment (getArgs)
import UseCase.FeedGenerator (showNewSinceLastVisit)
import View.CLIView (CLIView (CLIView))

query :: Text -> Text
query profile =
  "select id, score, created_at from scoreboard_"
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
    (const "Flatscraper RSS Feed") -- Title for the feed
