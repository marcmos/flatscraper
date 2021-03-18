{-# LANGUAGE OverloadedStrings #-}

module WordUtils
  ( stripSpaces
  , dropShitwords
  , parsePrice
  , parseExtras
  , parseRooms
  , parseInt
  ) where

import Data.Text as T
import Data.Text.Read (decimal)
import Data.List (filter)
import Data.Char (isDigit, digitToInt)
import Data.Either (fromRight)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

import Offer (OfferExtra(..))

stripSpaces :: Text -> Text
stripSpaces = T.unwords . T.words

shitwords :: [Text]
shitwords = T.pack <$> [
  "mieszkanie", "kraków", "piękne", "doskonałej", "lokalizacji", "nowe",
  "komfortowa", "komfortowe", "śliczna", "śliczne", "wynajmę", "piękna",
  "piękne", ",", "do", "wynajęcia", "wynajmie", "cicha", "okolica", "tania",
  "tanie", "duża", "świetnym",  "wykończeniu", "kompletne", "wyposażenie",
  "wynajme", "-", "dużym", "czyste", "czysta", "przytulna", "przytulne",
  "wynajem", "super", "wynajecia", "nowoczesną", "nowoczesne", "wszędzie",
  "blisko", "świetna", "ciche", "widokowe", "sypialnie", "przestronne",
  "2", "pokoje", "dwupokojowe", "pokojowe", "2-pok", "dwa",
  "2-pokojowe", "2-pokojowego",
  "dobra", "dobre", "dobrej",
  "jasne", "eleganckie", "funkcjonalne", "duże", "stylowe", "klimatyczne",
  "zadbane", "atrakcyjne",
  "apartament", "mieszkania", "budownictwo",
  "bezpośrednio", "właściciel",
  "centrum", "kraków", "krakowa",
  "wynajem", "wynajmu", "komunikacja",
  "osobne", "dla", "pary", "zaraz",
  "na", "w"
  ]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\w -> T.toLower w `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

parsePrice :: Text -> Int
parsePrice x =
  fromRight 0 $ fst <$>
  (decimal . T.takeWhile (',' /=) . T.concat . (Data.List.filter (T.any isDigit) <$> T.words)) x

regMatch :: Text -> Text -> Bool
regMatch expr = matchTest (regex expr)
  where regex = makeRegexOpts
          (CompOption False True True True False)
          defaultExecOpt

parseAnnex :: Text -> Bool
parseAnnex input = regMatch "aneks" input || regMatch "salon połączony" input ||
  regMatch "kuchnia połączona" input || regMatch "otwarta kuchnia" input

parseKeywords :: Text -> [Bool]
parseKeywords input =
  parseAnnex input :
  ((`matchTest` input) . makeRegexOpts compOptions defaultExecOpt . T.pack <$> [ "osobn"
      , "zmywar"
      , "internet"
      , "piekarnik"
      , "klimatyz"
      , "balkon"
      ])
  where compOptions = CompOption False True True True False

-- FIXME cancer
parseExtras :: Text -> [OfferExtra]
parseExtras input =
  [KitchenAnnex | annex] <>
  [SeparateRooms | rooms] <>
  [Dishwasher | dish] <>
  [Internet | internet] <>
  [Oven | oven] <>
  [Climatronic | climatronic] <>
  [Balcony | balcony]
  where [annex, rooms, dish, internet, oven, climatronic, balcony] = parseKeywords input

parseRooms :: Text -> Maybe Int
parseRooms "Kawalerka" = Just 1
parseRooms input = digitToInt <$> T.find isDigit input

parseInt :: Text -> Maybe Int
parseInt x = rightToMaybe $ fst <$> decimal x
  where rightToMaybe = either (const Nothing) Just
