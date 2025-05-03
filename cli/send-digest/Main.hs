module Main where

import Data.Text (Text)
import qualified Data.Text as T (isSuffixOf, pack, toLower)
import qualified Data.Text.Lazy as TL
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Domain.Offer (OfferView, _offerDetails, _offerDistrict)
import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (cmPricePerMeter),
    HTMLFeedPresenter (HTMLFeedPresenter),
    cmArea,
    defaultColorMapper,
  )
import System.Environment (getArgs)
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import UseCase.FeedGenerator (showNewSinceLastVisit)
import View.CLIView (CLIView (CLIView))
import View.SMTPView (SMTPView (SMTPView), loadCredentialsFromFile)

htmlCliView :: CLIView H.Html
htmlCliView = CLIView (TL.toStrict . H.renderHtml)

badgeColorMapper :: BadgeColorMapper
badgeColorMapper =
  defaultColorMapper
    { cmArea = Just $ \area -> if area >= 70 then "success" else "info",
      cmPricePerMeter = Just $ \ppm -> if ppm <= 12000 then "success" else "info"
    }

isPreferredDistrict :: Text -> Bool
isPreferredDistrict input =
  any
    (`T.isSuffixOf` normInput)
    preferredDistricts
  where
    normInput = T.toLower input
    preferredDistricts =
      [ "płaszów",
        "podgórze",
        "czyżyny",
        "grzegórzki",
        "stare miasto",
        -- tier 2
        "nowa wieś",
        "krowodrza",
        "prądnik czerwony",
        "prądnik biały"
      ]

offerFilter :: OfferView -> Bool
offerFilter o = maybe False isPreferredDistrict district
  where
    district = _offerDetails o >>= _offerDistrict

main :: IO ()
main = do
  smtpCreds <- loadCredentialsFromFile "smtp-creds"
  args <- getArgs

  let conf = do
        r <- case args of
          ["preview"] -> Nothing
          [r] -> Just $ T.pack r
          _ -> Just "example@gmail.com"
        creds <- smtpCreds
        Just (r, creds)

  case conf of
    Just (recipient, creds) ->
      showNewSinceLastVisit
        sqlite
        presenter
        (SMTPView (TL.toStrict . H.renderHtml) creds recipient)
        offFilter
    _ ->
      showNewSinceLastVisit
        sqlite
        presenter
        htmlCliView
        offFilter
  where
    sqlite = SQLitePersistence
    offFilter = Just offerFilter
    -- offFilter = Nothing
    presenter = HTMLFeedPresenter (Just badgeColorMapper)