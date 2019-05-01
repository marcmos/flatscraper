{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ScrapePersistence ( syncLastVisitTimestamps
                         ) where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Time
import Data.Text as T
import Offer

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OfferVisit
    url Text
    UniqueUrl url
    scrapeTimestamp UTCTime
    deriving Show
|]

syncLastVisitTimestamps :: UTCTime -> [Offer] -> IO [Offer]
syncLastVisitTimestamps timestamp offers = do
    x <- runSqlite "flatscraper.sqlite" $ do
           runMigration migrateAll
           mapM (insertBy . offerVisit) urls
    return $ Prelude.zipWith zipper x offers
  where urls = offerURL <$> offers
        offerVisit url = OfferVisit url timestamp
        zipper (Left e) r = r {offerVisit = Just $ (offerVisitScrapeTimestamp . entityVal) e}
        zipper (Right _) r = r {offerVisit = Just $ timestamp}
