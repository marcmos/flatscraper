{-# LANGUAGE OverloadedStrings #-}

module Newsfeed
  ( renderOfferFeed
  ) where

import Data.Text.Lazy as T
import Text.RSS.Syntax
import Text.RSS.Export

import Offer

renderOfferFeedEntry :: Offer -> RSSItem
renderOfferFeedEntry offer = (nullItem $ offerTitle offer)
  { rssItemLink = Just $ offerURL offer }

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe Text
renderOfferFeed offers =
  textRSS $ (nullRSS "" "") {rssChannel = (nullChannel "flatscraper" "")
                              {rssItems = renderOfferFeedEntry <$> offers}
                            }

