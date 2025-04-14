{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Persistence.SQLite (SQLitePersistence (SQLitePersistence)) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (Entity), Filter, SelectOpt (LimitTo), selectFirst, selectList, upsertBy, (=.), (==.))
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import UseCase.Offer (OfferDetails (OfferDetails, offerDescription, offerDistrict, offerStreet), OfferDetailsLoader (loadDetails), OfferSeeder (seedOffers), OfferView (OfferView, offerDetails, offerURL), offerRooms)
import UseCase.ScrapePersister (OfferStorer (storeOffers))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
OfferInstance
    createdAt UTCTime
    updatedAt UTCTime
    url Text
    UniqueUrl url
    title Text
    rooms Int Maybe
    area Double
    deriving Show

OfferPrice
  offerInstanceId OfferInstanceId
  created UTCTime
  price Int
  deriving Show
|]

data SQLitePersistence = SQLitePersistence

persistOffers :: [OfferView] -> IO ()
persistOffers offers = do
  time <- getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    forM_ offers $ \(OfferView url _ area title details) -> do
      upsertBy
        (UniqueUrl url)
        (OfferInstance time time url title (details >>= offerRooms) area)
        [ OfferInstanceUpdatedAt =. time,
          OfferInstanceTitle =. title,
          OfferInstanceRooms =. (details >>= offerRooms),
          OfferInstanceArea =. area
        ]

instance OfferStorer SQLitePersistence where
  storeOffers _ = persistOffers

instance OfferDetailsLoader SQLitePersistence where
  loadDetails _ offer =
    runSqlite "flatscraper.sqlite" $ do
      runMigration migrateAll
      maybe offer entityToRecord <$> entityQ
    where
      entityQ = selectFirst [OfferInstanceUrl ==. offerURL offer] []
      entityToRecord (Entity _ ent) =
        offer
          { offerURL = offerInstanceUrl ent,
            offerDetails =
              Just
                OfferDetails
                  { offerDescription = Just $ offerInstanceTitle ent, -- FIXME
                    offerRooms = offerInstanceRooms ent,
                    offerStreet = Nothing,
                    offerDistrict = Nothing
                  }
          }

instance OfferSeeder SQLitePersistence where
  seedOffers _ = liftIO $ loadRecentOffers 10

-- loadRecentOffers :: SQLitePersistence -> Int -> IO [Entity OfferInstance]
loadRecentOffers count = do
  offers <- runSqlite "flatscraper.sqlite" (selectList ([] :: [Filter OfferInstance]) [LimitTo count])
  mapM
    ( \(Entity _ (OfferInstance _ _ url title rooms area)) ->
        return $
          OfferView
            url
            0
            area
            title
            ( Just
                OfferDetails
                  { offerRooms = rooms,
                    offerDescription = Nothing,
                    offerStreet = Nothing,
                    offerDistrict = Nothing
                  }
            )
    )
    offers