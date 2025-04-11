module Main where

import Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter))
import UseCase.FeedGenerator (getFreshAndPresent)

main :: IO ()
main = do
  getFreshAndPresent cliPresenter
  where
    cliPresenter = CLIPresenter 1 2
