{-# LANGUAGE OverloadedStrings #-}

module Newsfeed
  ( renderOfferFeed
  ) where

import Data.Text as T (pack, intercalate, Text)
import qualified Data.Text.Lazy as TL (Text)
import Text.RSS.Syntax
import Text.RSS.Export
import Data.Time
import Data.Monoid ((<>))

import Offer

extraTag :: OfferExtra -> T.Text
extraTag KitchenAnnex = "anx"
extraTag SeparateRooms = "oso"
extraTag Dishwasher = "zmy"
extraTag Internet = "int"
extraTag Oven = "pie"
extraTag Climatronic = "kli"
extraTag Balcony = "bal"

offerTag :: Offer -> T.Text
offerTag offer =
  if null tags then "" else "[" <> T.intercalate "," tags <> "]"
  where tags =
          (maybe [] (return . T.pack . show) (offerRooms offer)) <>
          (if offerOwnerOffer offer == Just True then ["pry"] else []) <>
          (extraTag <$> offerExtras offer)

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
          <> " " <> offerTag offer <> " " <> offerTitle offer

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe TL.Text
renderOfferFeed offers =
  textRSS $ (nullRSS "" "") {rssChannel = (nullChannel "flatscraper" url)
                              {rssItems = renderOfferFeedEntry <$> offers}
                            }
  where url = "http://heap.memleak.pl/mmos/newsfeed.xml"

