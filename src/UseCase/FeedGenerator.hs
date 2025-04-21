{-# LANGUAGE OverloadedStrings #-}

module UseCase.FeedGenerator
  ( FeedPresenter (present),
    presentFeed,
    OfferFeedItem (..),
    OfferFeed (OfferFeed),
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Domain.Offer
  ( OfferDetails (_offerDescription, _offerDistrict, _offerStreet),
    OfferView
      ( _offerArea,
        _offerDetails,
        _offerLatestPrice,
        _offerTitle,
        _offerURL
      ),
    pricePerMeter,
  )
import UseCase.Offer (OfferSeeder (seedOffers))

class FeedPresenter fp where
  present :: fp -> OfferFeed -> IO ()

data OfferFeedItem = OfferFeedItem
  { offerURL :: Text,
    offerTitle :: Text,
    offerDescription :: Text,
    offerPrice :: Int,
    offerPricePerMeter :: Double,
    offerArea :: Double,
    offerStreet :: Maybe Text,
    offerDistrict :: Maybe Text
  }

newtype OfferFeed = OfferFeed [OfferFeedItem]

presentFeed :: (OfferSeeder os, FeedPresenter fp) => os -> fp -> IO ()
presentFeed seeder presenter = do
  offerViews <- seedOffers seeder
  let feedItems =
        ( \ov ->
            OfferFeedItem
              (_offerURL ov)
              (_offerTitle ov)
              (fromMaybe "" (_offerDetails ov >>= _offerDescription))
              (_offerLatestPrice ov)
              (_offerArea ov)
              (pricePerMeter ov)
              (_offerDetails ov >>= _offerStreet)
              (_offerDetails ov >>= _offerDistrict)
        )
          <$> offerViews
  present presenter (OfferFeed feedItems)