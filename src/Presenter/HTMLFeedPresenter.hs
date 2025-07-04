{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (BadgeColorMapper, cmArea, cmPrice, cmPricePerMeter),
    defaultColorMapper,
    HTMLFeedPresenter (HTMLFeedPresenter),
    v2Presenter,
    emailPresenter,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Domain.Offer
  ( BoolAttr (AirConditioning, Balcony),
    ElevatorGuess (..),
    HasElevator
      ( HasElevator,
        _hasElevatorGuess
      ),
    hasBoolAttrInAttrs,
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
import Text.Blaze.Html5.Attributes as A
  ( class_,
    href,
    style,
    target,
    title,
  )
import UseCase.FeedGenerator
import UseCase.GenerateTripSummary
  ( TripSummary
      ( lineNumbers,
        totalTripTime,
        totalWalkingTime,
        tripStartStopName
      ),
    closestHubName,
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

elevatorMarkup :: OfferFeedItem -> Maybe H.Html
elevatorMarkup ov =
  let elevator = offerHasElevator ov
      elevatorText = case elevator >>= _hasElevatorGuess of
        Just BuildingHasManyFloors -> Just "budynek ma 6+ pięter"
        Just BuildingNewAndHasFloors -> Just "nowy budynek z 5+ pięter"
        _ -> Nothing
   in elevator
        >>= ( \case
                HasElevator True Nothing -> Just $ H.toHtml ("winda" :: Text)
                HasElevator True (Just _) -> do
                  explanation <- elevatorText
                  Just $
                    H.abbr ! A.title explanation $
                      H.toHtml ("winda" :: Text)
                HasElevator False _ -> Nothing
            )

floorMarkup :: OfferFeedItem -> H.Html
floorMarkup ov =
  let ft = offerFloorText ov
      elevator = elevatorMarkup ov
      ft' =
        if isJust elevator
          then (<> ", ") <$> ft
          else ft
   in maybe
        (H.toHtml ("" :: Text))
        ( \tt ->
            let cls = case offerIsAccessible ov of
                  Just True -> "success"
                  Just False -> "danger"
                  Nothing -> "info"
             in badge cls $ Just $ do
                  H.toHtml tt
                  fromMaybe (H.toHtml ("" :: Text)) elevator
        )
        ft'

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

formattedBadge ::
  Formatters ->
  Maybe BadgeColorMapper ->
  (BadgeColorMapper -> Maybe (a -> Text)) ->
  a ->
  (Formatters -> a -> Text) ->
  H.Html
formattedBadge formatters colorMappers colorMapper value showText =
  badge
    (fromMaybe "" ((colorMappers >>= colorMapper) <*> Just value))
    (Just $ showText formatters value)

tripSummariesMarkup :: [TripSummary] -> H.Html
tripSummariesMarkup = \case
  [] -> H.toHtml ("" :: Text)
  summaries ->
    mapM_ renderTripSummaryBadge summaries
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
       in H.div ! A.class_ "p-1" $ badge badgeType (Just badgeText)

itemMarkup2 :: Formatters -> Maybe BadgeColorMapper -> OfferFeedItem -> H.Html
itemMarkup2
  formatters
  colorMapper
  ofi@OfferFeedItem
    { offerURL = url,
      offerArea = area,
      offerPrice = price,
      offerPricePerMeter = ppm,
      offerStreetText = street,
      offerMunicipalityArea = municipalityArea,
      offerDistrictText = district,
      offerTripSummaries = tripSummaries,
      offerMarket = market
    } = do
    H.div ! A.class_ "offer-item border p-2" $ do
      H.div ! A.class_ "p-1" $ do
        badge
          "info"
          ( case market of
              Just MarketPrimary -> Just "rynek: pierwotny" :: Maybe Text
              Just MarketSecondary -> Just "rynek: wtórny"
              Nothing -> Nothing
          )
        formattedBadge formatters colorMapper cmArea area areaText'
        formattedBadge formatters colorMapper cmPrice price priceText
        formattedBadge formatters colorMapper cmPricePerMeter ppm ppmText'
        mapM_
          ( \u -> do
              H.toHtml (" " :: Text)
              H.a
                ! A.href (H.toValue u)
                ! A.target "_blank"
                $ H.toHtml ("link" :: Text)
          )
          url
      H.div ! A.class_ "p-1" $ do
        badge "info" street
        badge "info" $ municipalityArea <|> district

      H.div ! A.class_ "p-1" $ do
        badge "info" (roomsText <$> offerRooms ofi)
        badge "info" (offerBuildYearText ofi)
        floorMarkup ofi
        boolAttrBadge AirConditioning "klimatyzacja" ofi
        boolAttrBadge Balcony "balkon" ofi

      tripSummariesMarkup tripSummaries

v2Presenter :: Maybe BadgeColorMapper -> HTMLFeedPresenter H.Html
v2Presenter colorMapper = HTMLFeedPresenter colorMapper $ \colorMapper' (OfferFeed formatters itemGroups) -> do
  return $ do
    H.div ! A.class_ "container" $
      mapM_
        ( \(groupTitle, offers) -> do
            H.h4 . H.toHtml $ groupTitle <> " (" <> (T.pack . show . length $ offers) <> ")"
            mapM_ (itemMarkup2 formatters colorMapper') offers
        )
        itemGroups

wrapPresenter :: HTMLFeedPresenter H.Html -> HTMLFeedPresenter H.Html
wrapPresenter (HTMLFeedPresenter colorMapper render) =
  HTMLFeedPresenter colorMapper $ \_ (OfferFeed formatters itemGroups) -> do
    css <- T.readFile "bootstrap.css"
    innerHtml <- render colorMapper (OfferFeed formatters itemGroups)
    return $ H.html $ do
      H.head $ H.style (H.toHtml css)
      H.body $
        H.div ! A.class_ "container" $
          innerHtml

emailPresenter :: Maybe BadgeColorMapper -> HTMLFeedPresenter H.Html
emailPresenter colorMapperM = wrapPresenter $ v2Presenter colorMapperM

instance FeedPresenter HTMLFeedPresenter H.Html where
  present :: HTMLFeedPresenter H.Html -> OfferFeed -> IO H.Html
  present (HTMLFeedPresenter colorMapper render) = render colorMapper

boolAttrBadge :: BoolAttr -> Text -> OfferFeedItem -> H.Html
boolAttrBadge attr label ov =
  badge "success" $ case hasBoolAttrInAttrs attr (offerBoolAttrs ov) of
    Just True -> Just label
    _ -> Nothing