{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ScrapePersistence ( loadPersistedDetails
                         , persistOffers
                         ) where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Time
import Data.Text as T
import Data.Maybe ()
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Offer (Offer(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OfferVisit
    scrapeTimestamp UTCTime
    url Text
    UniqueUrl url
    ownerRentPrice Int
    rentPrice Int Maybe
    deriving Show
|]

loadPersistedDetails :: [Offer] -> IO [Offer]
loadPersistedDetails offers = runSqlite "flatscraper.sqlite" $ do
  runMigration migrateAll
  mapM augment offers
  where entityQ offer = selectFirst [OfferVisitUrl ==. offerURL offer] []
        entityToRecord offer (Entity _ ent) = offer
          { offerVisit = offerVisitScrapeTimestamp ent
          , offerPrice = offerVisitOwnerRentPrice ent
          , offerRentPrice = offerVisitRentPrice ent
          , offerDetailed = True }
        augment offer = maybe offer (entityToRecord offer) <$> entityQ offer

persistOffers :: [Offer] -> IO ()
persistOffers offers = do
  timestamp <- liftIO getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    -- FIXME insertBy
    forM_ offers $ \offer -> insertBy $ OfferVisit timestamp (offerURL offer) (offerPrice offer)
      (offerRentPrice offer)
