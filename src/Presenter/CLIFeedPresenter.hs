{-# LANGUAGE InstanceSigs #-}

module Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter)) where

import UseCase.FeedGenerator (FeedPresenter (present), ResultFeed)

data CLIPresenter = CLIPresenter Int Double

instance FeedPresenter CLIPresenter where
  present :: CLIPresenter -> ResultFeed -> IO ()
  present (CLIPresenter i d) resultFeed = do
    print resultFeed
