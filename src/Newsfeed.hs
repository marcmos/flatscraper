{-# LANGUAGE OverloadedStrings #-}

module Newsfeed
  ( renderOfferFeed
  ) where

import Data.Text as T (pack)
import Data.Text.Lazy as TL
import Text.RSS.Syntax
import Text.RSS.Export
import Data.Time

import Offer

renderOfferFeedEntry :: Offer -> RSSItem
renderOfferFeedEntry offer = (nullItem $ offerTitle offer)
  { rssItemLink = Just $ offerURL offer
  , rssItemPubDate = (<> "+0200") <$> (T.pack . formatTime defaultTimeLocale rfc822DateFormat <$> offerVisit offer)
  }

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe Text
renderOfferFeed offers =
  textRSS $ (nullRSS "" "") {rssChannel = (nullChannel "flatscraper" "http://heap.memleak.pl/mmos/newsfeed.xml")
                              {rssItems = renderOfferFeedEntry <$> offers}
                            }
