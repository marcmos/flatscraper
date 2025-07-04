{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((>=>))
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf, pack, toLower)
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import DataAccess.SQLite (SQLiteOfferQuery, SQLitePersistence (SQLitePersistence))
import Domain.Offer
  ( OfferMarket (MarketPrimary),
    OfferView,
    _offerDetails,
    _offerDistrict,
    _offerMarket,
    _offerMunicipalityArea,
    _offerURL,
  )
import Prefs.Location (offerFilter)
import Prefs.Presenter (badgeColorMapper)
import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (cmPricePerMeter),
    HTMLFeedPresenter (HTMLFeedPresenter),
    cmArea,
    defaultColorMapper,
    emailPresenter,
    v2Presenter,
  )
import System.Environment (getArgs)
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import UseCase.FeedGenerator (LastVisitStorer (storeLastVisit), OfferFeedItem, getLastVisit, showNewSinceLastVisit)
import View.CLIView (CLIView (CLIView))
import View.SMTPView (SMTPView (SMTPView), loadCredentialsFromFile)

htmlCliView :: CLIView H.Html
htmlCliView = CLIView (TL.toStrict . H.renderHtml)

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
          ( \o (m, n) ->
              if ( offerFilter
                     (offerDistrict o)
                     (offerMunicipalityArea o)
                 )
                then (o : m, n)
                else (m, o : n)
          )
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
  where
    offerDistrict = _offerDetails >=> _offerDistrict
    offerMunicipalityArea = _offerDetails >=> _offerMunicipalityArea

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

  case conf of
    Just (recipient, creds) -> do
      currentTime <- getCurrentTime

      showNewSinceLastVisit
        sqlite
        presenter
        (SMTPView (TL.toStrict . H.renderHtml) creds recipient)
        (getLastVisit sqlite recipient)
        offerGroupper
        genTitle

      storeLastVisit sqlite recipient currentTime
    _ ->
      showNewSinceLastVisit
        sqlite
        presenter
        htmlCliView
        (return Nothing)
        offerGroupper
        genTitle
  where
    sqlite = SQLitePersistence
    presenter = emailPresenter (Just badgeColorMapper)