{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf, pack, toLower)
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Domain.Offer
  ( OfferMarket (MarketPrimary),
    OfferView,
    _offerDetails,
    _offerDistrict,
    _offerMarket,
    _offerMunicipalityArea,
    _offerURL,
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
import UseCase.FeedGenerator (LastVisitStorer (storeLastVisit), getLastVisit, showNewSinceLastVisit)
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

uninterestingMunicipalityAreas :: [Text]
uninterestingMunicipalityAreas =
  [ "bronowice wielkie",
    "tonie",
    "witkowice",
    "mydlniki",
    "bielany",
    "chełm",
    "las wolski",
    "olszanica",
    "przegorzały",
    "wola justowska",
    "zakamycze",
    "bodzów",
    "kobierzyn",
    "kostrze",
    "olszyny",
    "pychowice",
    "sidzina",
    "skotniki",
    "tyniec",
    "zakrzówek",
    "białe morza",
    "jugowice",
    "kliny",
    "kosocice",
    "opatkowice",
    "rajsko",
    "sobniowice",
    "swoszowice",
    "wróblewice",
    "zbydniowice",
    "kurdwanów",
    "piaski wielkie",
    "rżąka",
    "złocień",
    "rybitwy",
    "grębałów",
    "kantorowice",
    "lubocza",
    "łuczanowice",
    "wadów",
    "węgrzynowice",
    "zesławice",
    "branice",
    "kombinat",
    "kościelniki",
    "kujawy",
    "mogiła",
    "pleszów",
    "przylasek rusiecki",
    "ruszcza",
    "wolica",
    "wyróżenice",
    "wyciąże",
    "przewóz"
  ]

uninterestingDistricts :: [Text]
uninterestingDistricts =
  [ "swoszowice"
  ]

offerFilter :: Bool -> OfferView -> Bool
offerFilter _paranoid o =
  ( case muniArea of
      Just ma -> T.toLower ma `notElem` uninterestingMunicipalityAreas
      Nothing -> True
  )
    && ( case district of
           Just d -> T.toLower d `notElem` uninterestingDistricts
           Nothing -> True
       )
  where
    district = _offerDetails o >>= _offerDistrict
    muniArea = _offerDetails o >>= _offerMunicipalityArea

offerGroupper :: [OfferView] -> [(Text, [OfferView])]
offerGroupper offers =
  let (olxOffers, otherOffers) =
        foldr
          ( \o (olx, other) ->
              if "https://www.olx.pl" `T.isPrefixOf` _offerURL o
                then (o : olx, other)
                else (olx, o : other)
          )
          ([], [])
          offers
      (matchingOffers, nonMatchingOffers) =
        foldr
          (\o (m, n) -> if offerFilter True o then (o : m, n) else (m, o : n))
          ([], [])
          otherOffers
      (nonPrimaryMarketOffers, primaryMarketOffers) =
        foldr
          ( \o (wl, nl) ->
              if (_offerDetails o >>= _offerMarket) == Just MarketPrimary
                then (wl, o : nl)
                else (o : wl, nl)
          )
          ([], [])
          matchingOffers
   in filter
        (\(_, oList) -> not . null $ oList)
        [ ("Oferty z OLX", olxOffers),
          ("Oferty", nonPrimaryMarketOffers),
          ("Oferty z rynku pierwotnego", primaryMarketOffers),
          ("Oferty odrzucone", nonMatchingOffers)
        ]

simpleGroupper :: [OfferView] -> [(Text, [OfferView])]
simpleGroupper offers =
  [("Oferty spełniające kryteria", filter (offerFilter False) offers)]

genTitle :: [(Text, [OfferView])] -> Text
genTitle groups =
  "Specjalnie dla Ciebie przygotowałem "
    <> (T.pack . show . length $ offers)
    <> " ofert mieszkań do przeglądnięcia"
  where
    offers = concatMap snd groups

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

  let groupper = case conf of
        Just (addr, _) | addr == fallbackAddr -> offerGroupper
        Nothing -> offerGroupper
        _ -> simpleGroupper

  case conf of
    Just (recipient, creds) -> do
      currentTime <- getCurrentTime

      showNewSinceLastVisit
        sqlite
        presenter
        (SMTPView (TL.toStrict . H.renderHtml) creds recipient)
        (getLastVisit sqlite recipient)
        groupper
        genTitle

      storeLastVisit sqlite recipient currentTime
    _ ->
      showNewSinceLastVisit
        sqlite
        presenter
        htmlCliView
        (return Nothing)
        groupper
        genTitle
  where
    sqlite = SQLitePersistence
    presenter = HTMLFeedPresenter (Just badgeColorMapper)