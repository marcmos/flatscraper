module Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter)) where

import UseCase.DigestGenerator (FeedPresenter (present))
import UseCase.Offer ()

data CLIPresenter = CLIPresenter Int Double

instance FeedPresenter CLIPresenter where
  present (CLIPresenter i d) resultFeed = do
    print resultFeed
