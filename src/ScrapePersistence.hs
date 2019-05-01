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

import Control.Monad.IO.Class (liftIO)
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

syncLastVisitTimestamps :: LocalTime -> [Offer] -> IO [Offer]
syncLastVisitTimestamps timestamp offers = do
    -- tz <- getCurrentTimeZone
    tz <- return utc
    x <- runSqlite "/tmp/szkola_lite.sqlite" $ do
           runMigration migrateAll
           mapM (insertBy . (offerVisit tz)) urls
    return $ Prelude.zipWith (zipper tz) x offers
  where urls = offerURL <$> offers
        offerVisit tz url = OfferVisit url ((localTimeToUTC tz) timestamp)
        zipper tz (Left e) r = r {offerVisit = Just $ utcToLocalTime tz $ (offerVisitScrapeTimestamp . entityVal) e}
        zipper _ (Right _) r = r {offerVisit = Just $ timestamp}
