module WordUtils
  ( stripSpaces
  , dropShitwords
  ) where

import Data.Text as T

stripSpaces :: Text -> Text
stripSpaces = T.unwords . T.words

shitwords :: [Text]
shitwords = T.pack <$> ["mieszkanie", "kraków", "wynajmę"]

dropWords :: Foldable t => t Text -> Text -> Text
dropWords keywords x = T.unwords $ Prelude.filter (\w -> T.toLower w `notElem` keywords) $ T.words x

dropShitwords :: Text -> Text
dropShitwords = dropWords shitwords
