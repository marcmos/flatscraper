module Main where

import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import UseCase.FeedGenerator (presentFeed)

main :: IO ()
main = do
  let offerSeeder = SQLitePersistence
  let presenter = RSSFeedPresenter
  presentFeed offerSeeder presenter