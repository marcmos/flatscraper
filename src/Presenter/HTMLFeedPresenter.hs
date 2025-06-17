{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (BadgeColorMapper, cmArea, cmPrice, cmPricePerMeter),
    defaultColorMapper,
    HTMLFeedPresenter (HTMLFeedPresenter),
    v1,
    v2Presenter,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Domain.Offer
  ( ElevatorGuess (..),
    HasElevator
      ( HasElevator,
        _hasElevatorGuess
      ),
  )
import qualified Text.Blaze.Html as A
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
  ( Html,
    ToMarkup,
    a,
    abbr,
    body,
    div,
    h4,
    head,
    html,
    span,
    style,
    toHtml,
    toValue,
  )
import Text.Blaze.Html5.Attributes as A (class_, form, href, style, title)
import UseCase.FeedGenerator
import UseCase.FeedGenerator (OfferFeedItem (offerTripSummaries), OfferMarket (MarketPrimary, MarketSecondary))
import UseCase.GenerateTripSummary
  ( TripSummary
      ( lineNumbers,
        numberOfTransfers,
        totalTripTime,
        totalWalkingTime,
        tripStartStopName
      ),
    closestHubName,
    profileName,
  )

badge' :: (H.ToMarkup a) => Maybe a -> Maybe Text -> H.Html
badge' (Just t) (Just inlineStyle) = do
  do
    H.toHtml (" " :: Text)
    H.span
      ! A.class_ "badge"
      ! A.style (A.toValue inlineStyle)
      $ H.toHtml t
badge' (Just t) Nothing = do
  do
    H.toHtml (" " :: Text)
    H.span ! A.class_ "badge" $ H.toHtml t
badge' Nothing _ = H.toHtml ("" :: Text)

badge :: (H.ToMarkup a) => Text -> Maybe a -> H.Html
badge "info" t =
  badge'
    t
    (Just "color: #fff !important; background-color: #17a2b8 !important;")
badge "success" t =
  badge'
    t
    (Just "color: #fff !important; background-color: #28a745 !important;")
badge "danger" t =
  badge'
    t
    (Just "color: #fff !important; background-color: #dc3545 !important;")
badge _ t = badge "info" t

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
      offerStreetText = street,
      offerDistrictText = district,
      offerArea = area,
      offerPrice = price,
      offerPricePerMeter = ppm,
      offerRooms = rooms,
      offerMunicipalityArea = municipalityArea,
      offerTripSummaries = tripSummaries,
      offerMarket = market
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
                      Just $
                        H.abbr ! A.title explanation $
                          H.toHtml ("winda" :: Text)
                    HasElevator False _ -> Nothing
                )
        url = offerURL ov
        rowClass = "p-2"
        tripSummariesBadges = case tripSummaries of
          [] -> emptyNode
          summaries ->
            H.div $ mapM_ renderTripSummaryBadge summaries
          where
            renderTripSummaryBadge ts =
              let badgeType = if totalTripTime ts < 30 * 60 then "success" else "info"
                  badgeText =
                    toText (totalTripTime ts `div` 60)
                      <> " min. ("
                      <> toText (totalWalkingTime ts `div` 60)
                      <> " min. pieszo) do "
                      <> T.pack (closestHubName ts)
                      <> " z "
                      <> T.pack (tripStartStopName ts)
                      <> " (linie: "
                      <> T.intercalate ", " (map T.pack $ lineNumbers ts)
                      <> ")"
               in badge badgeType (Just badgeText)
    H.div ! A.class_ "border" $ do
      H.div ! A.class_ rowClass $ do
        badge
          "info"
          ( case market of
              Just MarketPrimary -> Just "Rynek: pierwotny" :: Maybe Text
              Just MarketSecondary -> Just "Rynek: wtórny"
              Nothing -> Nothing
          )
        badge
          (fromMaybe "" ((colorMapper >>= cmArea) <*> Just area))
          (Just $ areaText' formatters area)
        badge
          (fromMaybe "" ((colorMapper >>= cmPrice) <*> Just price))
          (Just $ priceText formatters price)
        badge
          (fromMaybe "" ((colorMapper >>= cmPricePerMeter) <*> Just ppm))
          (Just $ ppmText' formatters ppm)
        badge "info" (roomsText <$> rooms)

      H.div ! A.class_ rowClass $ do
        maybe
          emptyNode
          ( \tt ->
              let cls = case isAccessible of
                    Just True -> "success"
                    Just False -> "danger"
                    Nothing -> "info"
               in ( badge cls $ Just $ do
                      H.toHtml tt
                      maybe emptyNode (", " <>) elevatorMarkup
                  )
          )
          ft
        badge "info" (offerBuildYearText ov)
      H.div ! A.class_ rowClass $ do
        badge "info" street
        badge "info" district
        badge "success" municipalityArea
      H.div ! A.class_ rowClass $ do
        mapM_
          ( \u ->
              H.div $ H.a ! A.href (H.toValue u) $ H.toHtml (offerTitle ov)
          )
          url
      H.div ! A.class_ rowClass $ tripSummariesBadges

data BadgeColorMapper = BadgeColorMapper
  { cmArea :: Maybe (Double -> Text),
    cmPricePerMeter :: Maybe (Double -> Text),
    cmPrice :: Maybe (Int -> Text)
  }

defaultColorMapper :: BadgeColorMapper
defaultColorMapper = BadgeColorMapper Nothing Nothing Nothing

data HTMLFeedPresenter a
  = HTMLFeedPresenter
      (Maybe BadgeColorMapper)
      (Maybe BadgeColorMapper -> OfferFeed -> IO a)

v1Presenter :: Maybe BadgeColorMapper -> OfferFeed -> IO H.Html
v1Presenter colorMapper (OfferFeed formatters itemGroups) = do
  css <- T.readFile "bootstrap.css"
  return $ H.html $ do
    H.head $ H.style (H.toHtml css)
    H.body $
      H.div ! A.class_ "container" $
        mapM_
          ( \(groupTitle, offers) -> do
              H.h4 . H.toHtml $ groupTitle <> " (" <> (T.pack . show . length $ offers) <> ")"
              mapM_ (itemMarkup formatters colorMapper) offers
          )
          itemGroups

v1 :: Maybe BadgeColorMapper -> HTMLFeedPresenter H.Html
v1 colorMapper = HTMLFeedPresenter colorMapper v1Presenter

itemMarkup2 :: Formatters -> Maybe BadgeColorMapper -> OfferFeedItem -> H.Html
itemMarkup2
  formatters
  colorMapper
  OfferFeedItem
    { offerURL = url,
      offerArea = area,
      offerPrice = price,
      offerPricePerMeter = ppm,
      offerStreetText = street,
      offerMunicipalityArea = municipalityArea,
      offerDistrictText = district,
      offerTripSummaries = tripSummaries
    } = do
    H.div ! A.class_ "offer-item border p-2" $ do
      H.div ! A.class_ "p-1" $ do
        badge
          (fromMaybe "" ((colorMapper >>= cmArea) <*> Just area))
          (Just $ areaText' formatters area)
        badge
          (fromMaybe "" ((colorMapper >>= cmPrice) <*> Just price))
          (Just $ priceText formatters price)
        badge
          (fromMaybe "" ((colorMapper >>= cmPricePerMeter) <*> Just ppm))
          (Just $ ppmText' formatters ppm)
        mapM_ (\u -> H.a ! A.href (H.toValue u) $ H.toHtml ("link" :: Text)) url
      H.div ! A.class_ "p-1" $ do
        badge "info" street
        badge "info" $ municipalityArea <|> district
      tripSummariesBadges
    where
      tripSummariesBadges = case tripSummaries of
        [] -> emptyNode
        summaries ->
          mapM_ renderTripSummaryBadge summaries
        where
          emptyNode = H.toHtml ("" :: Text)
          renderTripSummaryBadge ts =
            let badgeType = if totalTripTime ts < 30 * 60 then "success" else "info"
                badgeText =
                  toText (totalTripTime ts `div` 60)
                    <> " min. ("
                    <> toText (totalWalkingTime ts `div` 60)
                    <> " min. pieszo) do "
                    <> T.pack (closestHubName ts)
                    <> " z "
                    <> T.pack (tripStartStopName ts)
                    <> " (linie: "
                    <> T.intercalate ", " (map T.pack $ lineNumbers ts)
                    <> ")"
             in H.div ! A.class_ "p-1" $ badge badgeType (Just badgeText)

v2Presenter :: Maybe BadgeColorMapper -> HTMLFeedPresenter H.Html
v2Presenter colorMapper = HTMLFeedPresenter colorMapper $ \colorMapper' (OfferFeed formatters itemGroups) -> do
  return $ do
    H.div ! A.class_ "container" $
      mapM_
        ( \(groupTitle, offers) -> do
            mapM_ (itemMarkup2 formatters colorMapper') offers
        )
        itemGroups

instance FeedPresenter HTMLFeedPresenter H.Html where
  present :: HTMLFeedPresenter H.Html -> OfferFeed -> IO H.Html
  present (HTMLFeedPresenter colorMapper render) = render colorMapper