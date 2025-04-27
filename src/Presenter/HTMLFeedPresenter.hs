{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Presenter.HTMLFeedPresenter
  ( HTMLPreviewPresenter (HTMLPreviewPresenter),
    BadgeColorMapper (BadgeColorMapper, cmArea, cmPrice, cmPricePerMeter),
    defaultColorMapper,
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as T (putStrLn)
import Domain.Offer (ElevatorGuess (..), HasElevator (HasElevator, _hasElevatorGuess))
import qualified Text.Blaze.Html as A
import Text.Blaze.Html.Renderer.Text as H (renderHtml)
import Text.Blaze.Html5 as H (Html, ToMarkup, a, abbr, div, span, toHtml, toValue, (!))
import Text.Blaze.Html5.Attributes as A (class_, href, title)
import UseCase.FeedGenerator

badge :: (ToMarkup a) => Maybe a -> Maybe Text -> Html
badge (Just t) (Just tag) = do
  H.toHtml (" " :: Text)
  H.span ! A.class_ (A.toValue $ "badge badge-" <> tag) $ H.toHtml t
badge (Just t) Nothing = badge (Just t) (Just "info")
badge Nothing _ = H.toHtml ("" :: Text)

infoSpan :: (ToMarkup a) => Maybe a -> Html
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
        colClass = "col-md-3 p-2"
    H.div ! A.class_ "row border" $ do
      H.div ! A.class_ "col-md-2 p-2" $ do
        badge (Just $ areaText' formatters area) ((colorMapper >>= cmArea) <*> Just area)
        badge (Just $ priceText formatters price) ((colorMapper >>= cmPrice) <*> Just price)
        badge (Just $ ppmText' formatters ppm) ((colorMapper >>= cmPricePerMeter) <*> Just ppm)
        infoSpan (roomsText <$> rooms)

      H.div ! A.class_ colClass $ do
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
      H.div ! A.class_ colClass $ do
        infoSpan locText
      H.div ! A.class_ "col-md-4 p-2" $ do
        H.a ! A.href (toValue url) $ H.toHtml (offerTitle ov)

renderDetails :: Formatters -> Maybe BadgeColorMapper -> OfferFeedItem -> TL.Text
renderDetails formatters cm item = H.renderHtml $ itemMarkup formatters cm item

data BadgeColorMapper = BadgeColorMapper
  { cmArea :: Maybe (Double -> Text),
    cmPricePerMeter :: Maybe (Double -> Text),
    cmPrice :: Maybe (Int -> Text)
  }

defaultColorMapper :: BadgeColorMapper
defaultColorMapper = BadgeColorMapper Nothing Nothing Nothing

newtype HTMLPreviewPresenter = HTMLPreviewPresenter (Maybe BadgeColorMapper)

instance FeedPresenter HTMLPreviewPresenter where
  present (HTMLPreviewPresenter colorMapper) (OfferFeed formatters items) = do
    T.putStrLn "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.css\">"
    T.putStrLn "<div class=\"container\">"
    forM_
      items
      ( \x -> do
          let r = renderDetails formatters colorMapper x
          T.putStrLn r
      )
    T.putStrLn "</div>"