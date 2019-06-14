module WordUtils
  ( stripSpaces
  , dropShitwords
  , parsePrice
  ) where

import Data.Text as T
import Data.Text.Read (decimal)
import Data.List (filter)
import Data.Char (isDigit)
import Data.Either (fromRight)

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
  "blisko", "świetna", "ciche", "widokowe", "sypialnie"
  ]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\w -> T.toLower w `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

parsePrice :: Text -> Int
parsePrice x =
  fromRight 0 $ fst <$>
  (decimal . T.takeWhile ((/=) ',') . T.concat . (Data.List.filter (T.any isDigit) <$> T.words)) x
