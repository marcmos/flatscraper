module Main where

import Data.Maybe (isNothing)
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
      cmPricePerMeter = Just $ \ppm -> if ppm <= 12000 then "success" else "info"
    }

knownDistricts :: [Text]
knownDistricts =
  [ "stare miasto",
    "grzegórzki",
    "prądnik czerwony",
    "prądnik biały",
    "krowodrza",
    "bronowice",
    "zwierzyniec",
    "dębniki",
    "łagiewniki-borek fałęcki",
    "swoszowice",
    "podgórze duchackie",
    "bieżanów-prokocim",
    "podgórze",
    "czyżyny",
    "mistrzejowice",
    "bieńczyce",
    "wzgórza krzesławickie",
    "nowa huta"
  ]

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