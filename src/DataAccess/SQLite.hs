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
import Data.Aeson (Value (String))
import Data.Aeson.Lens (AsNumber (_Integer))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (Entity), Filter, SelectOpt (Desc, LimitTo), selectFirst, selectList, upsertBy, (=.), (==.), (>.))
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Domain.Offer
  ( OfferDetails
      ( OfferDetails,
        _offerBuildingFloors,
        _offerBuiltYear,
        _offerDescription,
        _offerDistrict,
        _offerHasElevator,
        _offerPropertyFloor,
        _offerRooms,
        _offerStreet
      ),
    OfferView (OfferView, _offerArea, _offerLatestPrice, _offerURL),
    emptyDetails,
    _offerDetails,
  )
import UseCase.Offer (OfferSeeder (seedOffers), QueryAccess (getOffersCreatedAfter))
import UseCase.ScrapePersister (OfferDetailsLoader (loadDetails), OfferStorer (storeOffers))

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
    propertyFloor Int Maybe
    buildingFloors Int Maybe
    yearBuilt Int Maybe
    hasElevator Bool Maybe
    deriving Show

PropertyOfferTextAttribute
  createdAt UTCTime
  offerId OfferInstanceId
  name Text
  UniqueOfferName offerId name
  value Text
  deriving Show

-- OfferPrice
--   offerInstanceId OfferInstanceId
--   created UTCTime
--   price Int
--   deriving Show
|]

data SQLitePersistence = SQLitePersistence

upsertTextAttr offerId attrName attrValue time =
  case attrValue of
    Just newValue -> do
      _ <-
        upsertBy
          (UniqueOfferName offerId attrName)
          ( PropertyOfferTextAttribute
              time
              offerId
              attrName
              newValue
          )
          [PropertyOfferTextAttributeValue =. newValue]
      return ()
    Nothing -> return ()

persistOffers :: [OfferView] -> IO ()
persistOffers offers = do
  time <- getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    forM_ offers $ \(OfferView url price area title details) -> do
      let street = details >>= _offerStreet
      let district = details >>= _offerDistrict
      let pFloor = details >>= _offerPropertyFloor
      let buildingFloors = details >>= _offerBuildingFloors
      let builtYear = details >>= _offerBuiltYear
      let hasEl = details >>= _offerHasElevator

      e@(Entity offerId _) <-
        upsertBy
          (UniqueUrl url)
          ( OfferInstance
              time
              time
              url
              title
              (details >>= _offerRooms)
              area
              price
              street
              district
              pFloor
              buildingFloors
              builtYear
              hasEl
          )
          [ OfferInstanceUpdatedAt =. time,
            OfferInstanceTitle =. title,
            OfferInstanceRooms =. (details >>= _offerRooms),
            OfferInstanceArea =. area,
            OfferInstancePrice =. price,
            OfferInstanceStreet =. street,
            OfferInstanceDistrict =. district,
            OfferInstancePropertyFloor =. pFloor,
            OfferInstanceBuildingFloors =. buildingFloors,
            OfferInstanceYearBuilt =. builtYear,
            OfferInstanceHasElevator =. hasEl
          ]

      _ <- upsertTextAttr offerId "street" street time

      return e

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
                    _offerDistrict = offerInstanceDistrict ent,
                    _offerHasElevator = offerInstanceHasElevator ent,
                    _offerPropertyFloor = offerInstancePropertyFloor ent,
                    _offerBuildingFloors = offerInstanceBuildingFloors ent,
                    _offerBuiltYear = offerInstanceYearBuilt ent
                  }
          }

instance OfferSeeder SQLitePersistence where
  seedOffers _ = liftIO $ loadRecentOffers 20

loadRecentOffers :: (MonadUnliftIO m) => Int -> m [OfferView]
loadRecentOffers count = do
  offers <- runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    selectList ([] :: [Filter OfferInstance]) [LimitTo count]
  return $ map toOfferView offers

instance QueryAccess SQLitePersistence where
  getOffersCreatedAfter _ timestamp = do
    offers <-
      runSqlite "flatscraper.sqlite" $
        selectList
          [OfferInstanceCreatedAt >. timestamp]
          [Desc OfferInstanceCreatedAt]
    return $ map toOfferView offers

-- Internal
toOfferView :: Entity OfferInstance -> OfferView
toOfferView
  ( Entity
      _
      ( OfferInstance
          _
          _
          url
          title
          rooms
          area
          price
          street
          district
          pFloor
          bFloors
          bYear
          hasEl
        )
    ) =
    OfferView
      url
      price
      area
      title
      ( Just
          emptyDetails
            { _offerRooms = rooms,
              _offerDescription = Nothing,
              _offerStreet = street,
              _offerDistrict = district,
              _offerPropertyFloor = pFloor,
              _offerBuildingFloors = bFloors,
              _offerBuiltYear = bYear,
              _offerHasElevator = hasEl
            }
      )
