{-# LANGUAGE OverloadedStrings #-}

module Presenter.HTMLOffer () where

import Data.Text (Text)
import Text.Blaze.Html5 as H
import UseCase.FeedGenerator (OfferFeedItem)

renderDetails :: OfferFeedItem -> Text
renderDetails _ =
  "<td>\
  \</td>"