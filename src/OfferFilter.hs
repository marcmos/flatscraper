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
  , "prądnik"
  , "justowsk"
  , "kazimierz"
  , "hut"
  , "azor"
  ]

allFieldsTextPredicate :: [Text] -> Offer -> Bool
allFieldsTextPredicate banned offer = not . any (`isInfixOf` compareString) $ banned
    where compareString = toLower . offerTitle $ offer

dropBlacklisted :: [Offer] -> [Offer]
dropBlacklisted = filter (allFieldsTextPredicate bannedLocations)