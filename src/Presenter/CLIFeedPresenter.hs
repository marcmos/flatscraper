module Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter)) where

import Control.Monad (forM_)
import UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeed (OfferFeed),
    OfferFeedItem (offerURL),
  )

data CLIPresenter = CLIPresenter Int Double

-- instance FeedPresenter CLIPresenter where
--   present (CLIPresenter i d) (OfferFeed _ offers) = do
--     forM_ offers (print . offerURL)
