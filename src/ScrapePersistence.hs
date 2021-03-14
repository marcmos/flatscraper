{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module ScrapePersistence ( loadPersistedDetails
                         , persistOffers
                         ) where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Time
import Data.Text as T
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Offer (Offer(..), OfferExtra)

-- TODO rooms
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OfferVisit
    scrapeTimestamp UTCTime
    url Text
    UniqueUrl url
    ownerRentPrice Int
    filtered Text Maybe
    rentPrice Int Maybe
    area Int Maybe
    rooms Int Maybe
    region Text Maybe
    street Text Maybe
    ownerOffer Bool Maybe
    extras Text Maybe
    deriving Show
|]

parseExtras :: Text -> [OfferExtra]
parseExtras input = catMaybes $ readMaybe . T.unpack <$> T.splitOn "," input

loadPersistedDetails :: [Offer] -> IO [Offer]
loadPersistedDetails offers = runSqlite "flatscraper.sqlite" $ do
  runMigration migrateAll
  mapM augment offers
  where entityQ offer = selectFirst [OfferVisitUrl ==. offerURL offer] []
        entityToRecord offer (Entity _ ent) = offer
          { offerVisit = offerVisitScrapeTimestamp ent
          , offerPrice = offerVisitOwnerRentPrice ent
          , offerFiltered = offerVisitFiltered ent
          , offerRentPrice = offerVisitRentPrice ent
          , offerArea = offerVisitArea ent
          , offerRooms = offerVisitRooms ent
          , offerOwnerOffer = offerVisitOwnerOffer ent
          , offerExtras = maybe [] parseExtras (offerVisitExtras ent)
          , offerDetailed = True }
        augment offer = maybe offer (entityToRecord offer) <$> entityQ offer

setOfferVisitExtras :: Offer -> OfferVisit -> OfferVisit
setOfferVisitExtras offer offerVisit =
  offerVisit { offerVisitExtras = Just $ T.intercalate "," (T.pack . show <$> offerExtras offer) }

persistOffers :: [Offer] -> IO ()
persistOffers offers = do
  timestamp <- liftIO getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    -- FIXME insertBy
    forM_ offers $ \offer -> insertBy . setOfferVisitExtras offer $
      OfferVisit timestamp (offerURL offer) (offerPrice offer) (offerFiltered offer) (offerRentPrice offer)
      (offerArea offer) (offerRooms offer) Nothing Nothing (offerOwnerOffer offer) Nothing
