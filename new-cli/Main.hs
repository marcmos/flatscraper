module Main where

import UseCase.FeedGenerator (getFreshAndPresent)
import Presenter.CLIFeedPresenter (CLIPresenter(CLIPresenter))

main :: IO ()
main = do
    getFreshAndPresent cliPresenter
    where
        cliPresenter = CLIPresenter 1 2