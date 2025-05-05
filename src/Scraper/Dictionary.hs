module Scraper.Dictionary where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (splitOn, stripPrefix, toLower)

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

artificialDistricts :: [Text]
artificialDistricts =
  [ "śródmieście"
  ]

knownMunicipalityAreas :: [Text]
knownMunicipalityAreas =
  [ -- 1st district (Stare Miasto)
    "kazimierz",
    "nowy świat",
    "piasek",
    "przedmieście warszawskie",
    "stare miasto",
    "stradom",
    "wesoła",
    -- 2nd district (Grzegórzki)
    "dąbie",
    "grzegórzki",
    "os. oficerskie",
    "osiedle oficerskie",
    -- 3rd district (Prądnik Czerwony)
    "olsza",
    "prądnik czerwony",
    "rakowice",
    -- 4th district (Prądnik Biały)
    "azory",
    "bronowice wielkie",
    "górka narodowa",
    "krowodrza górka",
    "prądnik biały",
    "tonie",
    "witkowice",
    "żabiniec",
    -- 5th district (Krowodrza)
    "czarna wieś",
    "krowodrza",
    "łobzów",
    "małe błonia",
    "nowa wieś",
    -- 6th district (Bronowice)
    "bronowice",
    "bronowice małe",
    "mydlniki",
    -- 7th district (Zwierzyniec)
    "bielany",
    "błonia",
    "chełm",
    "las wolski",
    "olszanica",
    "przegorzały",
    "wola justowska",
    "zakamycze",
    "zwierzyniec",
    -- 8th district (Dębniki)
    "bodzów",
    "dębniki",
    "kobierzyn",
    "kostrze",
    "ludwinów",
    "olszyny",
    "pychowice",
    "ruczaj",
    "sidzina",
    "skotniki",
    "tyniec",
    "zakrzówek",
    -- 9th district (Łagiewniki-Borek Fałęcki)
    "białe morza",
    "borek fałęcki",
    "cegielniana",
    "łagiewniki",
    -- 10th district (Swoszowice)
    "jugowice",
    "kliny",
    "kosocice",
    "opatkowice",
    "rajsko",
    "soboniowice",
    "swoszowice",
    "wróblowice",
    "zbydniowice",
    -- 11th district (Podgórze Duchackie)
    "kurdwanów",
    "piaski wielkie",
    "wola duchacka",
    -- 12th district (Bieżanów-Prokocim)
    "bieżanów",
    "kozłówka",
    "prokocim",
    "rżąka",
    "złocień",
    -- 13th district (Podgórze)
    "kabel",
    "płaszów",
    "przewóz",
    "stare podgórze",
    "zabłocie",
    -- 14th district (Czyżyny)
    "czyżyny",
    "łęg",
    -- 15th district (Mistrzejowice)
    "mistrzejowice",
    -- 16th district (Bieńczyce)
    "bieńczyce",
    "zalew nowohucki",
    -- 17th district (Wzgórza Krzesławickie)
    "grębałów",
    "kantorowice",
    "krzesławice",
    "lubocza",
    "łuczanowice",
    "wadów",
    "węgrzynowice",
    "wzgórza krzesławickie",
    "zesławice",
    -- 18th district (Nowa Huta)
    "branice",
    "kombinat",
    "kościelniki",
    "kujawy",
    "mogiła",
    "nowa huta",
    "pleszów",
    "przylasek rusiecki",
    "ruszcza",
    "wolica",
    "wróżenice",
    "wyciąże"
  ]

-- FIXME city specific
parseLocationText :: Text -> (Maybe Text, Maybe Text, Maybe Text)
parseLocationText =
  parseLocationParts
    . reverse
    . map (\x -> fromMaybe x $ T.stripPrefix "Kraków-" x)
    . filter (\x -> x `notElem` ["Kraków", "Kraków M.", "małopolskie"])
    . T.splitOn ", "

parseLocationParts :: [Text] -> (Maybe Text, Maybe Text, Maybe Text)
parseLocationParts parts =
  let lookupDict dict x = (T.toLower x `elem` dict)
      knownDistrict x =
        lookupDict knownDistricts x
          || lookupDict artificialDistricts x
      knownMuni = lookupDict knownMunicipalityAreas
      probablyStreet s = (not . knownDistrict $ s) && (not . knownMuni $ s)
      tryMatch x [] = x
      tryMatch (Nothing, Nothing, Nothing) [x]
        | knownDistrict x =
            (Nothing, Nothing, Just x)
      tryMatch (Nothing, Nothing, Nothing) [x]
        | knownMuni x =
            (Nothing, Just x, Nothing)
      tryMatch (Nothing, Nothing, Nothing) [x] =
        (Just x, Nothing, Nothing)
      tryMatch (a, b, Nothing) (x : xs) | knownDistrict x = tryMatch (a, b, Just x) xs
      tryMatch (a, Nothing, c) (x : xs) | knownMuni x = tryMatch (a, Just x, c) xs
      tryMatch (Nothing, b, c) (x : xs) | probablyStreet x = tryMatch (Just x, b, c) xs
      tryMatch _ _ = (Nothing, Nothing, Nothing)
   in tryMatch (Nothing, Nothing, Nothing) parts