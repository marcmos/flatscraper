module Prefs.Location (offerFilter) where

import Data.Text (Text)
import qualified Data.Text as T

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

offerFilter :: Maybe Text -> Maybe Text -> Bool
offerFilter district municipality =
  ( case municipality of
      Just ma -> T.toLower ma `notElem` uninterestingMunicipalityAreas
      Nothing -> True
  )
    && ( case district of
           Just d -> T.toLower d `notElem` uninterestingDistricts
           Nothing -> True
       )
