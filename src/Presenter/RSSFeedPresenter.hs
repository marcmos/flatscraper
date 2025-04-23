{-# LANGUAGE OverloadedStrings #-}

module Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter)) where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSSChannel (rssItems), RSSItem (rssItemLink), nullChannel, nullItem, nullRSS, rssChannel)
import UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeed (OfferFeed),
    OfferFeedItem (offerDescription, offerURL),
  )

renderOffer :: OfferFeedItem -> RSSItem
renderOffer offer =
  (nullItem title)
    { rssItemLink = Just $ offerURL offer
    }
  where
    title = offerDescription offer

renderFeed :: OfferFeed -> Maybe TL.Text
renderFeed (OfferFeed offers) =
  textRSS $
    (nullRSS "" "")
      { rssChannel = (nullChannel "flatscraper" "") {rssItems = renderOffer <$> offers}
      }

data RSSFeedPresenter = RSSFeedPresenter

instance FeedPresenter RSSFeedPresenter where
  present RSSFeedPresenter offers = do
    let feed =
          fromMaybe
            "Failed to generate feed"
            (renderFeed offers)
    TL.putStrLn feed