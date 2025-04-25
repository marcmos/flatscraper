{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Presenter.HTMLFeedPresenter (HTMLPreviewPresenter (HTMLPreviewPresenter)) where

import Control.Monad (forM_)
import Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as T (putStrLn)
import Domain.Offer (ElevatorGuess (..), HasElevator (HasElevator, _hasElevatorGuess))
import qualified Text.Blaze.Html as A
import Text.Blaze.Html.Renderer.Text as H (renderHtml)
import Text.Blaze.Html5 as H (Html, ToMarkup, a, abbr, span, td, toHtml, tr, (!))
import Text.Blaze.Html5.Attributes as A (class_, href, title)
import UseCase.FeedGenerator
  ( FeedPresenter (present),
    OfferFeed (OfferFeed),
    OfferFeedItem
      ( OfferFeedItem,
        offerAreaText,
        offerBuildYearText,
        offerFloorText,
        offerHasElevator,
        offerIsAccessible,
        offerLocationText,
        offerPricePerAreaText,
        offerPriceText,
        offerTitle,
        offerURL
      ),
  )

infoSpan :: (ToMarkup a) => Maybe a -> Html
infoSpan (Just t) = do
  H.toHtml (" " :: Text)
  H.span ! A.class_ "badge badge-info" $ H.toHtml t
infoSpan Nothing = H.toHtml ("" :: Text)

itemMarkup :: OfferFeedItem -> H.Html
itemMarkup
  ov@OfferFeedItem
    { offerHasElevator = elevator,
      offerIsAccessible = isAccessible,
      offerFloorText = ft,
      offerLocationText = locText
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
    H.tr $ do
      H.td $ do
        infoSpan (offerAreaText ov)
        infoSpan (offerPriceText ov)
        infoSpan (offerPricePerAreaText ov)
      H.td $ do
        H.toHtml (offerTitle ov)
      H.td $ do
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
      H.td $ do
        infoSpan locText
      H.td $ do
        H.a ! A.href (A.toValue url) $ H.toHtml ("link" :: Text)

renderDetails :: OfferFeedItem -> TL.Text
renderDetails item = H.renderHtml $ itemMarkup item

data HTMLPreviewPresenter = HTMLPreviewPresenter

instance FeedPresenter HTMLPreviewPresenter where
  present _ (OfferFeed items) = do
    T.putStrLn "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.css\">"
    T.putStrLn "<div class=\"container\"><table class=\"table\"><tr><th>Oferta</th><th>Tytuł</th><th>Budynek</th><th>Lokalizacja</th><th>Link</th></tr>"
    forM_
      items
      ( \x -> do
          let r = renderDetails x
          T.putStrLn r
      )
    T.putStrLn "</table></div>"