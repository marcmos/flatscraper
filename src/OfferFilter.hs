{-# LANGUAGE OverloadedStrings          #-}

module OfferFilter where

import Data.Text (Text, isInfixOf, toLower, intercalate)
import Data.List (any)
import Data.Maybe (mapMaybe)

import Offer (Offer (offerDescription), offerTitle)

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
  -- , "rakowic"
  , "łobz"
  , "zwierzy" -- zwierzyniec
  , "prokoci"

  -- , "kazimierz" -- corner case: galeria kazimierz
  , "kazimierzu"
  , "kazimierza"

  , "stradom"
  , "starowiśln"

  , "kleparz"
  , "wrocławs"
  , "mazowiecka"
  , "wielick"

  -- , "centrum a", "centruma"
  -- , "centrum b", "centrumb" -- FIXME centrum biurowe
  -- , "centrum c", "centrumc"
  -- , "centrum d", "centrumd"

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

  , "agh"
  , " uj"
  ]

titleBlacklist :: [Text]
titleBlacklist = bannedLocations

descriptionBlacklist :: [Text]
descriptionBlacklist = bannedLocations

allFieldsTextPredicate :: [Text] -> Offer -> Bool
allFieldsTextPredicate banned offer = not . any (`isInfixOf` compareString) $ banned
    where compareString = toLower . offerTitle $ offer

dropBlacklisted :: [Offer] -> [Offer]
dropBlacklisted = filter (allFieldsTextPredicate bannedLocations)

data FieldFilter a = FieldFilter {
    fieldFilterFieldName :: Text
  , fieldFilterFieldExtractor :: Offer -> Maybe a
}

data OfferFilter = InfixFieldFilter (FieldFilter Text) [Text] |
                   RangeValueFieldFilter (FieldFilter Int)

titleBlacklistFilter :: OfferFilter
titleBlacklistFilter = InfixFieldFilter (FieldFilter "title" (return . offerTitle)) titleBlacklist

descriptionBlacklistFilter :: OfferFilter
descriptionBlacklistFilter = InfixFieldFilter (FieldFilter "description" offerDescription) descriptionBlacklist

runFilter :: OfferFilter -> Offer -> Maybe Text
runFilter (InfixFieldFilter (FieldFilter fieldName fieldExtractor) blacklist) offer = do
  fieldValue <- toLower <$> fieldExtractor offer
  let matchedWords = mapMaybe (\x -> if x `isInfixOf` fieldValue then Just x else Nothing) blacklist
  if null matchedWords then Nothing else Just (fieldName <> " blacklist match: " <> intercalate ", " matchedWords)
runFilter (RangeValueFieldFilter fieldFilter) offer = undefined

standardFilters :: [OfferFilter]
standardFilters = [titleBlacklistFilter, descriptionBlacklistFilter]

runFilters :: Offer -> Maybe Text
runFilters offer =
  if null matches then Nothing else Just $ intercalate ", " matches
  where matches = mapMaybe (`runFilter` offer) standardFilters
