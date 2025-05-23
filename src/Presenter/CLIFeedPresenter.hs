{-# LANGUAGE MultiParamTypeClasses #-}

module Presenter.CLIFeedPresenter (CLIPresenter (CLIPresenter)) where

import Data.Text (Text)
import qualified Data.Text as T
import UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeed (OfferFeed),
    OfferFeedItem (offerURL),
  )

data CLIPresenter a = CLIPresenter

instance FeedPresenter CLIPresenter Text where
  present CLIPresenter (OfferFeed _ offers) = do
    let o : _ = offers >>= (\(_, items) -> map offerURL items)
    return $ T.intercalate (T.pack "\n") o