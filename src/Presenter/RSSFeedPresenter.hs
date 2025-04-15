{-# LANGUAGE OverloadedStrings #-}

module Presenter.RSSFeedPresenter (RSSFeedPresenter (RSSFeedPresenter)) where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSSChannel (rssItems), RSSItem (rssItemLink), nullChannel, nullItem, nullRSS, rssChannel)
import UseCase.FeedGenerator (FeedPresenter (present))
import UseCase.Offer (OfferView (offerTitle, offerURL))

renderOffer :: OfferView -> RSSItem
renderOffer offer =
  (nullItem title)
    { rssItemLink = Just $ offerURL offer
    }
  where
    title = offerTitle offer

renderFeed :: [OfferView] -> Maybe TL.Text
renderFeed offers =
  textRSS $
    (nullRSS "" "")
      { rssChannel = (nullChannel "flatscraper" "") {rssItems = renderOffer <$> offers}
      }

data RSSFeedPresenter = RSSFeedPresenter

instance FeedPresenter RSSFeedPresenter where
  present _ offers = do
    let feed = fromMaybe "Failed to generate feed" (renderFeed offers)
    TL.putStrLn feed