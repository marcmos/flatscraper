{-# LANGUAGE FlexibleContexts #-}

module UseCase.EmergingOffer
  ( EmergingOfferSelector (..),
    selectEmergingOfferForRss,
  )
where

import Control.Monad (unless) -- for unless, can be removed if no debugging
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (liftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (PersistValue)
import Domain.Offer (OfferInstanceId, OfferView, _offerURL)
import System.IO (hPrint, hPutStrLn, stderr)

class (Monad m) => EmergingOfferSelector m where
  executeOfferSourceQuery :: Text -> [PersistValue] -> m [OfferInstanceId]
  filterSortAndLoadOffers ::
    Text ->
    [OfferInstanceId] ->
    m [OfferView]
  markAsRssPublished :: OfferInstanceId -> UTCTime -> Text -> Text -> m ()

selectEmergingOfferForRss ::
  (EmergingOfferSelector m, MonadIO m) =>
  Text ->
  -- | List of (SQL query text, parameters)
  [(Text, [PersistValue])] ->
  m (Maybe OfferView)
selectEmergingOfferForRss feedId offerSourceQueries = do
  go offerSourceQueries -- Pass them to go
  where
    go [] = do
      liftIO $ hPutStrLn stderr "DEBUG: No offer found after trying all query sources."
      return Nothing
    go ((sqlQuery, params) : qs) = do
      -- Use the passed values
      liftIO $ hPutStrLn stderr $ "DEBUG: Trying query source: " ++ T.unpack sqlQuery
      candidateIds <- executeOfferSourceQuery sqlQuery params
      liftIO $ hPutStrLn stderr $ "DEBUG: Candidate IDs from source query: " ++ show (length candidateIds)
      unless (null candidateIds) $ liftIO $ hPrint stderr candidateIds

      -- Use the 'currentRecentSince' and 'currentMinScoreM' passed to this iteration of 'go'
      offers <- filterSortAndLoadOffers feedId candidateIds
      liftIO $ hPutStrLn stderr $ "DEBUG: Offers after common filtering & sorting: " ++ show (length offers)
      unless (null offers) $ liftIO $ hPrint stderr (map _offerURL offers)

      case listToMaybe offers of
        Just offer -> do
          liftIO $ hPutStrLn stderr $ "DEBUG: Selected offer: " ++ T.unpack (_offerURL offer)
          return $ Just offer
        Nothing -> go qs -- Pass them along in the recursive call