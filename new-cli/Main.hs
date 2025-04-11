module Main where

import Persistence.DataAccess (DataAccess (DataAccess))
import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import UseCase.FeedGenerator (showNewSinceLastVisit)

main :: IO ()
main = do
  showNewSinceLastVisit loader cliPresenter 5
  where
    loader = DataAccess
    cliPresenter = CLIPresenter 1 2
