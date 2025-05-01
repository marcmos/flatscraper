{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DataAccess.SQLite (SQLitePersistence (SQLitePersistence)) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
  ( Entity (Entity),
    SelectOpt (Desc, LimitTo),
    selectFirst,
    selectList,
    upsertBy,
    (=.),
    (==.),
    (>.),
  )
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Domain.Offer
  ( OfferDetails
      ( _offerBuildingFloors,
        _offerBuiltYear,
        _offerDistrict,
        _offerHasElevator,
        _offerPropertyFloor,
        _offerRooms,
        _offerStreet
      ),
    OfferView (OfferView, _offerLatestPrice, _offerURL),
    emptyDetails,
    _offerDetails,
  )
import UseCase.Offer
  ( OfferSeeder (seedOffers),
    QueryAccess (getOffersCreatedAfter),
  )
import UseCase.ScrapePersister
  ( OfferDetailsLoader (loadDetails),
    OfferStorer (storeOffers),
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
OfferInstance
    createdAt UTCTime
    updatedAt UTCTime
    url Text
    UniqueUrl url
    title Text
    area Double
    price Int
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
              area
              price
          )
          [ OfferInstanceUpdatedAt =. time,
            OfferInstanceTitle =. title,
            OfferInstanceArea =. area,
            OfferInstancePrice =. price
          ]

      upsertTextAttr offerId "street" street time
      upsertTextAttr offerId "district" district time

      upsertIntAttr offerId "price" (Just price) time
      upsertIntAttr offerId "property_floor" pFloor time
      upsertIntAttr offerId "building_floors" buildingFloors time
      upsertIntAttr offerId "building_year" builtYear time
      upsertIntAttr offerId "property_rooms" (details >>= _offerRooms) time
      upsertIntAttr
        offerId
        "building_has_elevator"
        ( case hasEl of
            Just True -> Just 1
            Just False -> Just 0
            _ -> Nothing
        )
        time

      return e

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
                "building_has_elevator" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerHasElevator = Just $ value > 0})
                    }
                _ -> acc
    )

toOfferView :: OfferInstance -> OfferView
toOfferView
  ( OfferInstance
      { offerInstanceUrl = url,
        offerInstancePrice = price,
        offerInstanceArea = area,
        offerInstanceTitle = title
      }
    ) =
    OfferView url price area title (Just emptyDetails)

-- loadAttributes :: Entity OfferInstance -> OfferView
loadAttributes (Entity offerId offerInstance) = do
  let offer = toOfferView offerInstance
  textAttrs <- selectList [OfferTextAttributeOfferId ==. offerId] []
  let offer' = loadTextAttrs offer textAttrs
  intAttrs <- selectList [OfferIntegralAttributeOfferId ==. offerId] []
  let offer'' = loadIntegralAttrs offer' intAttrs
  return offer''

getCreatedAfter timestamp limit =
  runSqlite "flatscraper.sqlite" $ do
    offers <-
      selectList
        [OfferInstanceCreatedAt >. timestamp]
        [Desc OfferInstanceCreatedAt, LimitTo limit]

    mapM loadAttributes offers

instance OfferSeeder SQLitePersistence where
  seedOffers :: SQLitePersistence -> IO [OfferView]
  seedOffers _ = do
    let limit = 20
    yesterday <- addUTCTime (-(24 * 3600)) <$> getCurrentTime
    getCreatedAfter yesterday limit

instance QueryAccess SQLitePersistence where
  getOffersCreatedAfter :: SQLitePersistence -> UTCTime -> IO [OfferView]
  getOffersCreatedAfter _ timestamp =
    let limit = 100
     in getCreatedAfter timestamp limit

instance OfferStorer SQLitePersistence where
  storeOffers _ = persistOffers

instance OfferDetailsLoader SQLitePersistence where
  loadDetails _ offer =
    runSqlite "flatscraper.sqlite" $ do
      runMigration migrateAll
      persistedOffer <- entityQ
      case persistedOffer of
        Nothing -> return offer
        Just o -> loadAttributes o
    where
      entityQ = selectFirst [OfferInstanceUrl ==. _offerURL offer] []
