{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Domain.WordUtils where

import Data.Char (digitToInt, isAlphaNum, isDigit)
import Data.Either (fromRight)
import Data.List (filter, maximumBy, minimumBy, nub)
import Data.Maybe (mapMaybe)
import Data.Text as T
import Data.Text.Read (decimal)
import Domain.Offer (Offer (offerRentPrice), OfferExtra (..))
import Text.RE.TDFA.Text
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

stripSpaces :: Text -> Text
stripSpaces = T.unwords . T.words

shitwords :: [Text]
shitwords =
  T.pack
    <$> [ "mieszkanie",
          "kraków",
          "piękne",
          "doskonałej",
          "lokalizacji",
          "nowe",
          "komfortowa",
          "komfortowe",
          "śliczna",
          "śliczne",
          "wynajmę",
          "piękna",
          "piękne",
          ",",
          "do",
          "wynajęcia",
          "wynajmie",
          "cicha",
          "okolica",
          "tania",
          "tanie",
          "duża",
          "świetnym",
          "wykończeniu",
          "kompletne",
          "wyposażenie",
          "wynajme",
          "-",
          "dużym",
          "czyste",
          "czysta",
          "przytulna",
          "przytulne",
          "wynajem",
          "super",
          "wynajecia",
          "nowoczesną",
          "nowoczesne",
          "wszędzie",
          "blisko",
          "świetna",
          "ciche",
          "widokowe",
          "sypialnie",
          "przestronne",
          "2",
          "pokoje",
          "dwupokojowe",
          "pokojowe",
          "2-pok",
          "dwa",
          "2-pokojowe",
          "2-pokojowego",
          "dobra",
          "dobre",
          "dobrej",
          "jasne",
          "eleganckie",
          "funkcjonalne",
          "duże",
          "stylowe",
          "klimatyczne",
          "zadbane",
          "atrakcyjne",
          "apartament",
          "mieszkania",
          "budownictwo",
          "bezpośrednio",
          "właściciel",
          "centrum",
          "kraków",
          "krakowa",
          "wynajem",
          "wynajmu",
          "komunikacja",
          "osobne",
          "dla",
          "pary",
          "zaraz",
          "na",
          "w"
        ]

dropWords :: (Foldable t) => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\w -> T.toLower w `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

parsePrice :: Text -> Int
parsePrice x =
  fromRight 0 $
    fst
      <$> (decimal . T.takeWhile (',' /=) . T.concat . (Data.List.filter (T.any isDigit) <$> T.words)) x

regMatch :: Text -> Text -> Bool
regMatch expr = matchTest (regex expr)
  where
    regex =
      makeRegexOpts
        (CompOption False True True True False)
        defaultExecOpt

parseAnnex :: Text -> Bool
parseAnnex input =
  regMatch "aneks" input
    || regMatch "salon połączony" input
    || regMatch "kuchnia połączona" input
    || regMatch "otwarta kuchnia" input

parseKeywords :: Text -> [Bool]
parseKeywords input =
  parseAnnex input
    : ( (`matchTest` input) . makeRegexOpts compOptions defaultExecOpt . T.pack
          <$> [ "osobn",
                "zmywar",
                "internet",
                "piekarnik",
                "klimatyz",
                "balkon"
              ]
      )
  where
    compOptions = CompOption False True True True False

-- FIXME cancer
parseExtras :: Text -> [OfferExtra]
parseExtras input =
  [KitchenAnnex | annex]
    <> [SeparateRooms | rooms]
    <> [Dishwasher | dish]
    <> [Internet | internet]
    <> [Oven | oven]
    <> [Climatronic | climatronic]
    <> [Balcony | balcony]
  where
    [annex, rooms, dish, internet, oven, climatronic, balcony] = parseKeywords input

parseRooms :: Text -> Maybe Int
parseRooms "Kawalerka" = Just 1
parseRooms input = digitToInt <$> T.find isDigit input

parseInt :: Text -> Maybe Int
parseInt x = rightToMaybe $ fst <$> decimal x
  where
    rightToMaybe = either (const Nothing) Just

tokenizeReplaceMap :: Char -> Char
tokenizeReplaceMap c
  | (not . isAlphaNum) c = ' '
  | otherwise = c

tokenizeDescription :: Text -> [Text]
tokenizeDescription = T.words . T.map tokenizeReplaceMap

numbers t = mapMaybe parseInt (tokenizeDescription t)

priceInRange :: Int -> Int -> Bool
priceInRange area rentPrice = rentPrice >= area * 5 && rentPrice <= area * 15 && rentPrice `mod` 5 == 0

normalizeDescription :: Text -> Text
normalizeDescription = normalizeNumbers . removePhone

possibleRentPrices :: Int -> Text -> [Int]
possibleRentPrices area = Data.List.filter (priceInRange area) . nub . numbers . normalizeDescription

pickRentPrice :: Int -> [Int] -> Maybe Int
pickRentPrice _ [x] = Just x
pickRentPrice _ [] = Nothing
pickRentPrice area xs = Just $ minimumBy (\x y -> compare (abs (x - area * 10)) (abs (y - area * 10))) xs

possibleRentPrice :: Int -> Text -> Maybe Int
possibleRentPrice area desc = pickRentPrice area $ possibleRentPrices area desc

removePhone :: Text -> Text
removePhone = (*=~/ [ed|( ?(\+48 ?)?[0-9]{3}[ -]?[0-9]{3}[ -]?[0-9]{3})///|])

normalizeNumbers :: Text -> Text
normalizeNumbers = (*=~/ [ed|([a-zA-Z-]+)([0-9]+)///$1 $2|]) . (*=~/ [ed|([1-9])[., ]([0-9]{3})///$1$2|])
