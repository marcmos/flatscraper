{-# LANGUAGE OverloadedStrings #-}

module Presenter.Newsfeed
  ( renderOfferFeed,
  )
where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, intercalate, pack, concat)
import qualified Data.Text.Lazy as TL (Text)
import Data.Time
import Data.List (intersperse)
import Domain.Offer
import Text.RSS.Export
import Text.RSS.Syntax

extraTag :: OfferExtra -> T.Text
extraTag KitchenAnnex = "anx"
extraTag SeparateRooms = "oso"
extraTag Dishwasher = "zmy"
extraTag Internet = "int"
extraTag Oven = "pie"
extraTag Climatronic = "kli"
extraTag Balcony = "bal"

ownerTag :: Offer -> [T.Text]
ownerTag Offer {offerOwnerOffer = Just True} = ["pry"]
ownerTag Offer {offerOwnerOffer = Just False} = ["age"]
ownerTag _ = []

extraDescription :: OfferExtra -> T.Text
extraDescription KitchenAnnex = "kuchnia z aneksem"
extraDescription SeparateRooms = "coś jest osobne (pokoje?)"
extraDescription Dishwasher = "zmywarka"
extraDescription Oven = "piekarnik"
extraDescription Climatronic = "klimatyzacja"
extraDescription Balcony = "balkon"
extraDescription Internet = "internet"

ownerDescription :: Offer -> [T.Text]
ownerDescription Offer {offerOwnerOffer = Just True} = ["oferta prywatna"]
ownerDescription Offer {offerOwnerOffer = Just False} = ["oferta z biura/agencji"]
ownerDescription _ = []

offerTag :: Offer -> T.Text
offerTag offer =
  if null tags then "" else "[" <> T.intercalate "," tags <> "]"
  where
    tags =
      maybe [] (return . T.pack . show) (offerRooms offer)
        <> ownerTag offer
        <> (extraTag <$> offerExtras offer)

agencyFee :: Offer -> Maybe Int
agencyFee offer = do
  False <- offerOwnerOffer offer
  -- return $ offerPrice offer `div` 12
  Nothing

pricePlusFee :: Offer -> Maybe Int
pricePlusFee offer = (offerPrice offer +) <$> agencyFee offer

monthlyPrice :: Offer -> Maybe Int
monthlyPrice offer = do
  rentPrice <- offerRentPrice offer
  let price = fromMaybe (offerPrice offer) (pricePlusFee offer)
  return $ price + rentPrice

offerDescriptionTexts :: Offer -> [T.Text]
offerDescriptionTexts offer =
  -- ["Czynsz: " <> (T.pack . show) (offerPrice offer)] <>
  -- maybe [] (return . ("odstępne: " <>) . T.pack . show) (offerRentPrice offer) <>
  -- maybe [] (return . ("narzut agencji (estymowany): " <>) . T.pack . show) (agencyFee offer) <>
  -- maybe [] (return . ("cena za mc: " <>) . T.pack . show) (monthlyPrice offer) <>
  ownerDescription offer
    <> maybe [] return (roomsDescription offer)
    <> maybe [] return (areaDescription offer)
    <> (extraDescription <$> filter (/= SeparateRooms) (offerExtras offer))

roomsDescription :: Offer -> Maybe T.Text
roomsDescription offer =
  (\p -> "pokoje" <> (if SeparateRooms `elem` offerExtras offer then " osobne(?): " else ": ") <> p)
    . (T.pack . show)
    <$> offerRooms offer

areaDescription :: Offer -> Maybe T.Text
areaDescription offer = (\x -> T.pack "metraż: " <> (T.pack . show) x) <$> offerArea offer

areaTag :: Offer -> T.Text
areaTag offer =
  " ["
    <> T.intercalate
      ", "
      ( catMaybes
          [ (<> "m2") . T.pack . show <$> offerArea offer,
            pricePerSquareMeter offer,
            Just . attrText $ offer
          ]
      )
    <> "] "

attrText :: Offer -> T.Text
attrText = T.pack . show . length . offerExtras

pricePerSquareMeter :: Offer -> Maybe T.Text
pricePerSquareMeter offer = (\area -> T.pack . show $ price `div` area) <$> offerArea offer
  where
    price = offerPrice offer

renderPrice :: Offer -> T.Text
renderPrice offer =
  if totalPrice == ownerPrice
    then (T.pack . show) ownerPrice
    else T.concat $ (intersperse (T.pack "+") $ (T.pack . show) <$> priceParts) <> ["=", T.pack . show $ totalPrice]
  where ownerPrice = offerPrice offer
        rentPrice = offerRentPrice offer
        agencyFee = if offerOwnerOffer offer == Just False then Just (ownerPrice `div` 12) else Nothing
        priceParts = catMaybes $ Just ownerPrice : rentPrice : agencyFee : []
        totalPrice = sum priceParts

renderOfferFeedEntry :: Offer -> RSSItem
renderOfferFeedEntry offer =
  (nullItem title)
    { rssItemLink = Just $ offerURL offer,
      rssItemPubDate = Just . T.pack . formatTime defaultTimeLocale rfc822DateFormat . offerVisit $ offer,
      rssItemDescription = Just . T.intercalate ", " $ offerDescriptionTexts offer,
      rssItemAuthor = Just $ offerScraperName offer
    }
  where
    price = offerPrice offer
    title =
        renderPrice offer
        <> areaTag offer
        <> offerTitle offer

-- FIXME nullRSS empty strings replaced later
renderOfferFeed :: [Offer] -> Maybe TL.Text
renderOfferFeed offers =
  textRSS $
    (nullRSS "" "")
      { rssChannel =
          (nullChannel "flatscraper" url)
            { rssItems = renderOfferFeedEntry <$> offers
            }
      }
  where
    url = "http://heap.memleak.pl/mmos/newsfeed.xml"
