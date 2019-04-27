{-# LANGUAGE OverloadedStrings #-}

module Newsfeed
  ( renderOfferFeed
  ) where

import Offer
import Data.Text.Lazy.IO as T
import Data.Text.Lazy as T
import Text.RSS.Syntax
import Text.RSS.Export
import OtodomScraper
import GratkaScraper
import Control.Monad
import Data.Maybe
import Data.List (concat)

renderOfferFeedEntry :: Offer -> RSSItem
renderOfferFeedEntry offer = (nullItem $ Offer.title offer)
  { rssItemLink = Just $ Offer.url offer }

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe Text
renderOfferFeed offers =
  textRSS $ (nullRSS "" "") {rssChannel = (nullChannel "flatscraper" "") {rssItems = renderOfferFeedEntry <$> offers}}

