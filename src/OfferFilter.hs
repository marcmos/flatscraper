{-# LANGUAGE OverloadedStrings          #-}

module OfferFilter where

import Data.Text (Text, isInfixOf, toLower, intercalate, pack)
import Data.List (any)
import Data.Maybe (mapMaybe)

import Offer (Offer (offerDescription, offerArea), offerTitle)

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
  , "na kazimierzu"

  , "stradom"
  , "starowiśln"

  , "kleparz"
  , "wrocławs"
  , "mazowieck"
  , "wielick"
  , "matecznego"

  -- , "centrum a", "centruma"
  -- , "centrum b", "centrumb" -- FIXME centrum biurowe
  -- , "centrum c", "centrumc"
  -- , "centrum d", "centrumd"

  , "huta"
  , "hucie"

  , "borek"
  , "fałę"
  , "łagiew"

  , "błoń"
  , "błoni"

  , "czerwone maki"
  , "czerwonych maków"
  , "czerwonym makom"
  , "czerwonymi makami"
  , "czerwonych makach"

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
  -- , " uj"
  ]

titleBlacklist :: [Text]
titleBlacklist = bannedLocations ++ [
  --  "kazimierz"
    "salwator" -- biuro nieruchomości SALWATOR
  ]

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
                   RangeValueFieldFilter (FieldFilter Int) Int

titleBlacklistFilter :: OfferFilter
titleBlacklistFilter = InfixFieldFilter (FieldFilter "title" (return . offerTitle)) titleBlacklist

descriptionBlacklistFilter :: OfferFilter
descriptionBlacklistFilter = InfixFieldFilter (FieldFilter "description" offerDescription) descriptionBlacklist

minimalAreaFilter :: Int -> OfferFilter
minimalAreaFilter = RangeValueFieldFilter (FieldFilter "area" offerArea)

runFilter :: OfferFilter -> Offer -> Maybe Text
runFilter (InfixFieldFilter (FieldFilter fieldName fieldExtractor) blacklist) offer = do
  fieldValue <- toLower <$> fieldExtractor offer
  let matchedWords = mapMaybe (\x -> if x `isInfixOf` fieldValue then Just x else Nothing) blacklist
  if null matchedWords then Nothing else Just (fieldName <> " blacklist match: " <> intercalate ", " matchedWords)
runFilter (RangeValueFieldFilter (FieldFilter fieldName fieldExtractor) minValue) offer = do
  fieldValue <- fieldExtractor offer
  if fieldValue < minValue
    then Just (fieldName <> " value " <> (pack . show) fieldValue <> " smaller than " <> (pack . show) minValue)
    else Nothing

standardFilters :: [OfferFilter]
standardFilters = [titleBlacklistFilter, descriptionBlacklistFilter, minimalAreaFilter 35]

runFilters :: Offer -> Maybe Text
runFilters offer =
  if null matches then Nothing else Just $ intercalate ", " matches
  where matches = mapMaybe (`runFilter` offer) standardFilters
