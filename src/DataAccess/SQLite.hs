{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
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
import Control.Monad.Zip (mzip)
import Data.Aeson (Value (String), object)
import Data.Aeson.Lens (AsNumber (_Integer))
import Data.Maybe (fromJust, fromMaybe)
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
import Language.Haskell.TH.Lens (HasName (name))
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

OfferTextAttribute
  createdAt UTCTime
  offerId OfferInstanceId
  name Text
  UniqueOfferName offerId name
  value Text
  deriving Show

OfferIntegralAttribute
  createdAt UTCTime
  offerId OfferInstanceId
  name Text
  UniqueOfferName2 offerId name
  value Int
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
          ( OfferTextAttribute
              time
              offerId
              attrName
              newValue
          )
          [OfferTextAttributeValue =. newValue]
      return ()
    Nothing -> return ()

upsertIntAttr offerId attrName attrValue time =
  case attrValue of
    Just newValue -> do
      _ <-
        upsertBy
          (UniqueOfferName2 offerId attrName)
          ( OfferIntegralAttribute
              time
              offerId
              attrName
              newValue
          )
          [OfferIntegralAttributeValue =. newValue]
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
              Nothing
              area
              price
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
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

      upsertTextAttr offerId "street" street time
      upsertTextAttr offerId "district" district time

      upsertIntAttr offerId "price" (Just price) time
      upsertIntAttr offerId "property_floor" pFloor time
      upsertIntAttr offerId "building_floors" buildingFloors time
      upsertIntAttr offerId "building_year" builtYear time
      upsertIntAttr offerId "property_rooms" (details >>= _offerRooms) time

      return e

instance OfferStorer SQLitePersistence where
  storeOffers _ = persistOffers

instance OfferDetailsLoader SQLitePersistence where
  loadDetails _ offer = return offer

-- runSqlite "flatscraper.sqlite" $ do
--   runMigration migrateAll
--     maybe offer toOfferView <$> entityQ
--   where
--   entityQ = selectFirst [OfferInstanceUrl ==. _offerURL offer] []

-- entityToRecord (Entity _ ent) =
--   offer
--     { _offerURL = offerInstanceUrl ent,
--       _offerLatestPrice = offerInstancePrice ent,
--       _offerArea = offerInstanceArea ent,
--       _offerDetails =
--         Just
--           OfferDetails
--             { _offerDescription = Just $ offerInstanceTitle ent, -- FIXME
--               _offerRooms = offerInstanceRooms ent,
--               _offerStreet = offerInstanceStreet ent,
--               _offerDistrict = offerInstanceDistrict ent,
--               _offerHasElevator = offerInstanceHasElevator ent,
--               _offerPropertyFloor = offerInstancePropertyFloor ent,
--               _offerBuildingFloors = offerInstanceBuildingFloors ent,
--               _offerBuiltYear = offerInstanceYearBuilt ent
--             }
--     }

instance OfferSeeder SQLitePersistence where
  seedOffers :: SQLitePersistence -> IO [OfferView]
  seedOffers _ = liftIO $ loadRecentOffers 20

loadRecentOffers :: (MonadUnliftIO m) => Int -> m [OfferView]
loadRecentOffers count = do
  offers <- runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    selectList ([] :: [Filter OfferInstance]) [LimitTo count]
  -- return $ map toOfferView offers
  undefined

loadTextAttrs ::
  OfferView ->
  [Entity OfferTextAttribute] ->
  OfferView
loadTextAttrs =
  foldr
    ( \(Entity _ (OfferTextAttribute _ _ attrName value))
       acc ->
          let details = fromMaybe emptyDetails (_offerDetails acc)
           in case attrName of
                "street" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerStreet = Just value})
                    }
                "district" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerDistrict = Just value})
                    }
                _ -> acc
    )

loadIntegralAttrs ::
  OfferView ->
  [Entity OfferIntegralAttribute] ->
  OfferView
loadIntegralAttrs =
  foldr
    ( \(Entity _ (OfferIntegralAttribute _ _ attrName value))
       acc ->
          let details = fromMaybe emptyDetails (_offerDetails acc)
           in case attrName of
                "price" ->
                  acc
                    { _offerLatestPrice = value
                    }
                "property_floor" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerPropertyFloor = Just value})
                    }
                "property_rooms" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerRooms = Just value})
                    }
                "building_floors" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerBuildingFloors = Just value})
                    }
                "building_year" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerBuiltYear = Just value})
                    }
                _ -> acc
    )

instance QueryAccess SQLitePersistence where
  getOffersCreatedAfter :: SQLitePersistence -> UTCTime -> IO [OfferView]
  getOffersCreatedAfter _ timestamp = do
    runSqlite "flatscraper.sqlite" $ do
      o <-
        selectList
          -- [OfferInstanceCreatedAt >. timestamp]
          []
          [Desc OfferInstanceCreatedAt]

      let offerViews =
            map
              ( \( Entity
                     _
                     ( OfferInstance
                         { offerInstanceUrl = url,
                           offerInstancePrice = price
                         }
                       )
                   ) -> OfferView url price 0 "" (Just emptyDetails)
              )
              o

      textAttrs <-
        mapM
          ( \(Entity offerId _) ->
              selectList [OfferTextAttributeOfferId ==. offerId] []
          )
          o

      let offers = zipWith loadTextAttrs offerViews textAttrs

      intAttrs <-
        mapM
          ( \(Entity offerId _) ->
              selectList [OfferIntegralAttributeOfferId ==. offerId] []
          )
          o

      let offers' = zipWith loadIntegralAttrs offers intAttrs

      return offers'

-- Internal
-- toOfferView :: Entity OfferInstance -> [Entity PropertyOfferTextAttribute] -> OfferView
-- toOfferView
--   ( Entity
--       _
--       ( OfferInstance
--           _
--           _
--           url
--           title
--           rooms
--           area
--           price
--           street
--           district
--           pFloor
--           bFloors
--           bYear
--           hasEl
--         )
--     ) =
--     OfferView
--       url
--       price
--       area
--       title
--       ( Just
--           emptyDetails
--             { _offerRooms = rooms,
--               _offerDescription = Nothing,
--               _offerStreet = street,
--               _offerDistrict = district,
--               _offerPropertyFloor = pFloor,
--               _offerBuildingFloors = bFloors,
--               _offerBuiltYear = bYear,
--               _offerHasElevator = hasEl
--             }
--       )
