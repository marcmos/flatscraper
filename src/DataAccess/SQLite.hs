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

module DataAccess.SQLite (SQLitePersistence (SQLitePersistence)) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (Entity), Filter, SelectOpt (LimitTo), selectFirst, selectList, upsertBy, (=.), (==.))
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import UseCase.Offer
  ( OfferDetails (OfferDetails, _offerDescription, _offerDistrict, _offerRooms, _offerStreet),
    OfferDetailsLoader (loadDetails),
    OfferSeeder (seedOffers),
    OfferView (OfferView, _offerArea, _offerDetails, _offerLatestPrice, _offerURL),
    offerRooms,
  )
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
    price Int
    street Text Maybe
    district Text Maybe
    deriving Show

-- OfferPrice
--   offerInstanceId OfferInstanceId
--   created UTCTime
--   price Int
--   deriving Show
|]

data SQLitePersistence = SQLitePersistence

persistOffers :: [OfferView] -> IO ()
persistOffers offers = do
  time <- getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    forM_ offers $ \(OfferView url price area title details) -> do
      let street = details >>= _offerStreet
      let district = details >>= _offerDistrict
      upsertBy
        (UniqueUrl url)
        (OfferInstance time time url title (details >>= _offerRooms) area price street district)
        [ OfferInstanceUpdatedAt =. time,
          OfferInstanceTitle =. title,
          OfferInstanceRooms =. (details >>= _offerRooms),
          OfferInstanceArea =. area,
          OfferInstancePrice =. price,
          OfferInstanceStreet =. street,
          OfferInstanceDistrict =. district
        ]

instance OfferStorer SQLitePersistence where
  storeOffers _ = persistOffers

instance OfferDetailsLoader SQLitePersistence where
  loadDetails _ offer =
    runSqlite "flatscraper.sqlite" $ do
      runMigration migrateAll
      maybe offer entityToRecord <$> entityQ
    where
      entityQ = selectFirst [OfferInstanceUrl ==. _offerURL offer] []
      entityToRecord (Entity _ ent) =
        offer
          { _offerURL = offerInstanceUrl ent,
            _offerLatestPrice = offerInstancePrice ent,
            _offerArea = offerInstanceArea ent,
            _offerDetails =
              Just
                OfferDetails
                  { _offerDescription = Just $ offerInstanceTitle ent, -- FIXME
                    _offerRooms = offerInstanceRooms ent,
                    _offerStreet = offerInstanceStreet ent,
                    _offerDistrict = offerInstanceDistrict ent
                  }
          }

instance OfferSeeder SQLitePersistence where
  seedOffers _ = liftIO $ loadRecentOffers 20

loadRecentOffers :: (MonadUnliftIO m) => Int -> m [OfferView]
loadRecentOffers count = do
  offers <- runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    selectList ([] :: [Filter OfferInstance]) [LimitTo count]
  mapM
    ( \(Entity _ (OfferInstance _ _ url title rooms area price street district)) ->
        return $
          OfferView
            url
            price
            area
            title
            ( Just
                OfferDetails
                  { _offerRooms = rooms,
                    _offerDescription = Nothing,
                    _offerStreet = street,
                    _offerDistrict = district
                  }
            )
    )
    offers