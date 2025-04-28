module Main where

import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter))
import UseCase.FeedGenerator (showNewSinceLastVisit)

main :: IO ()
main = do
  let offerSeeder = SQLitePersistence
  let presenter = RSSFeedPresenter
  undefined

-- showNewSinceLastVisit offerSeeder presenter