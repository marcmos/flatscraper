{-# LANGUAGE OverloadedStrings #-}

module Newsfeed
  ( renderOfferFeed
  ) where

import Data.Text as T (pack)
import Data.Text.Lazy as TL (Text)
import Text.RSS.Syntax
import Text.RSS.Export
import Data.Time
import Data.Monoid ((<>))

import Offer

renderOfferFeedEntry :: Offer -> RSSItem
renderOfferFeedEntry offer = (nullItem title)
  { rssItemLink = Just $ offerURL offer
  , rssItemPubDate = Just . T.pack . formatTime defaultTimeLocale rfc822DateFormat . offerVisit $ offer
  , rssItemDescription = Just $ offerTitle offer
  , rssItemAuthor = Just $ offerScraperName offer
  }
  where
    price = offerPrice offer
    title = (case offerRentPrice offer of
          Just rentPrice ->
            (T.pack . show) price <> "+" <>
            (T.pack . show) rentPrice <> "=" <>
            (T.pack . show) (price + rentPrice)
          Nothing        -> T.pack . show $ offerPrice offer)
          <> " " <> offerTitle offer

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe Text
renderOfferFeed offers =
  textRSS $ (nullRSS "" "") {rssChannel = (nullChannel "flatscraper" url)
                              {rssItems = renderOfferFeedEntry <$> offers}
                            }
  where url = "http://heap.memleak.pl/mmos/newsfeed.xml"

