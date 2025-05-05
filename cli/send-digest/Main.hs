{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isInfixOf, isSuffixOf, pack, toLower)
import qualified Data.Text.Lazy as TL
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Domain.Offer
  ( OfferView,
    _offerDetails,
    _offerDistrict,
    _offerMunicipalityArea,
  )
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
      cmPricePerMeter = Just $ \case
        x | x < 10000 -> "danger"
        x | x <= 12000 -> "success"
        _ -> "info"
    }

interestingDistricts :: [Text]
interestingDistricts =
  [ "stare miasto",
    "grzegórzki",
    "krowodrza",
    "czyżyny",
    "mistrzejowice",
    "bieńczyce"
  ]

interestingMunicipalityAreas :: [Text]
interestingMunicipalityAreas =
  [ -- no doubt
    -- 1st district
    "kazimierz",
    "nowy świat",
    "piasek",
    "przedmieście warszawskie",
    "stare miasto",
    "stradom",
    "wesoła",
    -- 2nd district
    "dąbie",
    "grzegórzki",
    "os. oficerskie",
    "osiedle oficerskie",
    -- 3rd district
    "rakowice",
    -- 4th district
    "krowodrza górka",
    -- 5th district
    "czarna wieś",
    "krowodrza",
    "łobzów",
    "małe błonia",
    "nowa wieś",
    -- 6th district
    "bronowice",
    -- 7th district
    "zwierzyniec",
    -- 8th district
    "dębniki",
    "ludwinów",
    -- 9th-12th district
    -- nothing
    -- 13th district
    "kabel",
    "płaszów",
    "stare podgórze",
    "zabłocie",
    -- 14th district
    "czyżyny",
    "łęg",
    -- 15th district
    "mistrzejowice",
    -- 16th district
    "bieńczyce",
    "zalew nowohucki",
    -- 17th district
    -- nothing
    -- 18th district
    "nowa huta"
  ]

isPreferredDistrict :: Text -> Bool
isPreferredDistrict input =
  any
    (`T.isInfixOf` normInput)
    interestingDistricts
  where
    normInput = T.toLower input

isPreferredArea :: Text -> Bool
isPreferredArea input =
  any
    (`T.isInfixOf` normInput)
    interestingMunicipalityAreas
  where
    normInput = T.toLower input

offerFilter :: OfferView -> Bool
offerFilter o =
  maybe False isPreferredDistrict district
    || maybe False isPreferredArea muniArea
    -- include offers that have no district and muni area info
    -- because of fomo
    || (isNothing district && isNothing muniArea)
  where
    district = _offerDetails o >>= _offerDistrict
    muniArea = _offerDetails o >>= _offerMunicipalityArea

main :: IO ()
main = do
  smtpCreds <- loadCredentialsFromFile "smtp-creds"
  args <- getArgs

  let fallbackAddr = "marcmospl@gmail.com"
  let conf = do
        r <- case args of
          ["preview"] -> Nothing
          [_, "preview"] -> Nothing
          ["filtered"] -> Just fallbackAddr
          ["filtered", r] -> Just $ T.pack r
          [r] -> Just $ T.pack r
          _ -> Just fallbackAddr
        creds <- smtpCreds
        Just (r, creds)

  let selectedFilter = case args of
        "filtered" : _ -> not . offerFilter
        _ -> offerFilter

  case conf of
    Just (recipient, creds) ->
      showNewSinceLastVisit
        sqlite
        presenter
        (SMTPView (TL.toStrict . H.renderHtml) creds recipient)
        (Just selectedFilter)
    _ ->
      showNewSinceLastVisit
        sqlite
        presenter
        htmlCliView
        (Just selectedFilter)
  where
    sqlite = SQLitePersistence
    presenter = HTMLFeedPresenter (Just badgeColorMapper)