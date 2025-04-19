module Main where

import qualified Data.Text.ICU as Locale (LocaleName (Locale))
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import UseCase.FeedGenerator (presentFeed)

main :: IO ()
main = do
  let offerSeeder = SQLitePersistence
  let locale = Locale.Locale "pl_PL"
  let presenter = RSSFeedPresenter locale
  presentFeed offerSeeder presenter