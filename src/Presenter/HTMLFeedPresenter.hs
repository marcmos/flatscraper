{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (BadgeColorMapper, cmArea, cmPrice, cmPricePerMeter),
    defaultColorMapper,
    HTMLFeedPresenter (HTMLFeedPresenter),
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Domain.Offer (ElevatorGuess (..), HasElevator (HasElevator, _hasElevatorGuess))
import qualified Text.Blaze.Html as A
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
  ( Html,
    ToMarkup,
    a,
    abbr,
    body,
    div,
    head,
    html,
    span,
    style,
    toHtml,
    toValue,
  )
import Text.Blaze.Html5.Attributes as A (class_, href, title)
import UseCase.FeedGenerator

badge :: (H.ToMarkup a) => Maybe a -> Maybe Text -> H.Html
badge (Just t) (Just tag) = do
  H.toHtml (" " :: Text)
  H.span ! A.class_ (A.toValue $ "badge badge-" <> tag) $ H.toHtml t
badge (Just t) Nothing = badge (Just t) (Just "info")
badge Nothing _ = H.toHtml ("" :: Text)

infoSpan :: (H.ToMarkup a) => Maybe a -> H.Html
infoSpan t = badge t Nothing

roomsText :: Int -> Text
roomsText 0 = "0 pokoi?"
roomsText 1 = "1 pokój"
roomsText n | n < 5 = toText n <> " pokoje"
roomsText n = toText n <> " pokoi"

itemMarkup :: Formatters -> Maybe BadgeColorMapper -> OfferFeedItem -> H.Html
itemMarkup
  formatters
  colorMapper
  ov@OfferFeedItem
    { offerHasElevator = elevator,
      offerIsAccessible = isAccessible,
      offerFloorText = ft,
      offerLocationText = locText,
      offerArea = area,
      offerPrice = price,
      offerPricePerMeter = ppm,
      offerRooms = rooms
    } = do
    let emptyNode = H.toHtml ("" :: Text)
        elevatorText = case elevator >>= _hasElevatorGuess of
          Just BuildingHasManyFloors -> Just "budynek ma 6+ pięter"
          Just BuildingNewAndHasFloors -> Just "nowy budynek z 5+ pięter"
          _ -> Nothing
        elevatorMarkup =
          elevator
            >>= ( \case
                    HasElevator True Nothing -> Just $ H.toHtml ("winda" :: Text)
                    HasElevator True (Just _) -> do
                      explanation <- elevatorText
                      Just $ H.abbr ! A.title explanation $ H.toHtml ("winda" :: Text)
                    HasElevator False _ -> Nothing
                )
        url = offerURL ov
        rowClass = "p-2"
    H.div ! A.class_ "border" $ do
      H.div ! A.class_ rowClass $ do
        badge (Just $ areaText' formatters area) ((colorMapper >>= cmArea) <*> Just area)
        badge (Just $ priceText formatters price) ((colorMapper >>= cmPrice) <*> Just price)
        badge (Just $ ppmText' formatters ppm) ((colorMapper >>= cmPricePerMeter) <*> Just ppm)
        infoSpan (roomsText <$> rooms)

      H.div ! A.class_ rowClass $ do
        maybe
          emptyNode
          ( \tt ->
              let cls = case isAccessible of
                    Just True -> "badge badge-success"
                    Just False -> "badge badge-danger"
                    Nothing -> "badge badge-info"
               in ( H.span ! A.class_ cls $ do
                      H.toHtml tt
                      maybe emptyNode (", " <>) elevatorMarkup
                  )
          )
          ft
        infoSpan (offerBuildYearText ov)
      H.div ! A.class_ rowClass $ do
        infoSpan locText
      H.div ! A.class_ rowClass $ do
        H.a ! A.href (H.toValue url) $ H.toHtml (offerTitle ov)

data BadgeColorMapper = BadgeColorMapper
  { cmArea :: Maybe (Double -> Text),
    cmPricePerMeter :: Maybe (Double -> Text),
    cmPrice :: Maybe (Int -> Text)
  }

defaultColorMapper :: BadgeColorMapper
defaultColorMapper = BadgeColorMapper Nothing Nothing Nothing

newtype HTMLFeedPresenter a = HTMLFeedPresenter (Maybe BadgeColorMapper)

instance FeedPresenter HTMLFeedPresenter H.Html where
  present (HTMLFeedPresenter colorMapper) (OfferFeed formatters items) = do
    css <- T.readFile "bootstrap.css"
    return $ H.html $ do
      H.head $ H.style (H.toHtml css)
      H.body $
        H.div ! A.class_ "container" $
          mapM_ (\x -> do itemMarkup formatters colorMapper x) items
