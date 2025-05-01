{-# LANGUAGE MultiParamTypeClasses #-}

module Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter)) where

import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text, toStrict)
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax
  ( RSS,
    RSSChannel (rssItems),
    RSSItem (rssItemLink),
    nullChannel,
    nullItem,
    nullRSS,
    rssChannel,
  )
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

rss :: [OfferFeedItem] -> RSS
rss offers =
  (nullRSS "" "")
    { rssChannel =
        (nullChannel "flatscraper" "")
          { rssItems = renderOffer <$> offers
          }
    }

renderFeed :: OfferFeed -> Maybe TL.Text
renderFeed (OfferFeed _ offers) = textRSS $ rss offers

data RSSFeedPresenter a = RSSFeedPresenter

instance FeedPresenter RSSFeedPresenter RSS where
  present RSSFeedPresenter (OfferFeed _ offers) = do
    return $ rss offers

instance FeedPresenter RSSFeedPresenter T.Text where
  present RSSFeedPresenter feed =
    return $ maybe "Failed to generate feed" TL.toStrict (renderFeed feed)
