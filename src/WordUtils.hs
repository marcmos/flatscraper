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
shitwords = T.pack <$> ["mieszkanie", "kraków", "wynajmę"]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\w -> T.toLower w `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords

parsePrice :: Text -> Int
parsePrice x =
  fromRight 0 $ fst <$>
  (decimal . T.takeWhile ((/=) ',') . T.concat . (Data.List.filter (T.any isDigit) <$> T.words)) x
