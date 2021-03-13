{-# LANGUAGE OverloadedStrings          #-}

module OfferFilter where

import Data.Text (Text, isInfixOf, toLower)
import Data.List (any)

import Offer (Offer, offerTitle)

bannedLocations :: [Text]
bannedLocations = [
    "ruczaj"
  , "bronowic"
  , "krowodrz"
  , "dębnik"
  , "prądni"
  , "justowsk"
  , "azor"
  , "kurdwan"
  , "salwator"
  , "duchack"
  , "stare miasto" -- use only for title filtering
  , "rakowic"
  , "łobz"
  , "zwierzy" -- zwierzyniec
  , "prokoci"

  , "kazimierz" -- corner case: galeria kazimierz
  , "stradom"
  , "starowiśln"

  , "kleparz"
  , "wrocławs"
  , "mazowiecka"

  , "centrum a", "centruma"
  , "centrum b", "centrumb"
  , "centrum c", "centrumc"
  , "centrum d", "centrumd"

  , " hut"
  , " hucie"
  , "borek"
  , "fałę"
  , "łagiew"

  , "błoń"
  , "błoni"

  , " mak"
  , "europejski"

  , "29 listopada"
  , "opolsk"
  , "prandoty"
  , "olsz"
  , "żabi"
  , "dobrego pasterza"

  , "podwawels"

  , "krzeszow"

  , "quattro"
  , "nokia"
  , "motorola"
  , "serenad"

  ]

allFieldsTextPredicate :: [Text] -> Offer -> Bool
allFieldsTextPredicate banned offer = not . any (`isInfixOf` compareString) $ banned
    where compareString = toLower . offerTitle $ offer

dropBlacklisted :: [Offer] -> [Offer]
dropBlacklisted = filter (allFieldsTextPredicate bannedLocations)