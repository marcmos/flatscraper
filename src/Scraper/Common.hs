module Scraper.Common
  ( parsePrice,
    parsePrice',
    parseDecimal,
    parseDouble,
  )
where

import Data.Char (isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

parsePrice :: Text -> Int
parsePrice x =
  either
    (const 0)
    fst
    ( ( T.decimal
          . T.takeWhile (',' /=)
          . T.concat
          . (filter (T.any isDigit) <$> T.words)
      )
        x
    )

parsePrice' :: Text -> Maybe Int
parsePrice' x =
  fst
    <$> rightToMaybe
      ( T.decimal
          . T.takeWhile (',' /=)
          . T.concat
          . (filter (T.any isDigit) <$> T.words)
          $ x
      )

parseDecimal :: Text -> Maybe Int
parseDecimal t = fst <$> rightToMaybe (T.decimal t)

parseDouble :: Text -> Maybe Double
parseDouble t = fst <$> (rightToMaybe . T.double . T.replace "," ".") t