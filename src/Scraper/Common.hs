module Scraper.Common
  ( parsePrice,
    parsePrice',
  )
where

import Data.Char (isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Domain.Offer
  ( OfferDetails,
    emptyDetails,
    emptyOffer,
  )

parsePrice :: Text -> Int
parsePrice x =
  either (const 0) fst ((T.decimal . T.takeWhile (',' /=) . T.concat . (filter (T.any isDigit) <$> T.words)) x)

parsePrice' :: Text -> Maybe Int
parsePrice' x =
  fst <$> rightToMaybe (T.decimal . T.takeWhile (',' /=) . T.concat . (filter (T.any isDigit) <$> T.words) $ x)

-- defaultDetails :: Maybe OfferDetails -> OfferDetails
-- defaultDetails = fromMaybe emptyDetails (_offerDetails emptyOffer)