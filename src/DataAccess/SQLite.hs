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

import Control.Monad (filterM, forM, forM_, (<=<))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
  ( Entity (Entity),
    SelectOpt (Asc, Desc, LimitTo),
    insert,
    insert_,
    selectFirst,
    selectList,
    upsertBy,
    (=.),
    (==.),
    (>.),
  )
import Database.Persist.Sql (SqlPersistM, runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Domain.Offer
  ( OfferCoordinates (OfferExactCoordinates, _latitude, _longitude),
    OfferDetails
      ( _offerBuildingFloors,
        _offerBuiltYear,
        _offerCoordinates,
        _offerDistrict,
        _offerHasElevator,
        _offerMarket,
        _offerMunicipalityArea,
        _offerPropertyFloor,
        _offerRooms,
        _offerStreet
      ),
    OfferMarket (MarketPrimary, MarketSecondary),
    OfferView (OfferView, _offerLatestPrice, _offerURL),
    emptyDetails,
    _offerDetails,
  )
import UseCase.FeedGenerator
  ( LastVisitStorer (getLastVisit, storeLastVisit),
  )
import qualified UseCase.GenerateTripSummary as UC
  ( TripSummary
      ( TripSummary,
        closestHubName,
        lineNumbers,
        numberOfTransfers,
        profileName,
        totalTripTime,
        totalWalkingTime,
        tripStartStopName,
        tripStartTime
      ),
    TripSummaryDataAccess (..),
  )
import UseCase.Offer
  ( OfferSeeder (seedOffers),
    QueryAccess (fetchTripSummaries, getOffersCreatedAfter),
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

OfferFloatingAttribute
  createdAt UTCTime
  offerId OfferInstanceId
  name Text
  UniqueOfferName3 offerId name
  value Double
  deriving Show

-- OfferPrice
--   offerInstanceId OfferInstanceId
--   created UTCTime
--   price Int
--   deriving Show

Visit
  user Text
  UniqueUserVisit user
  time UTCTime
  deriving Show

TripSummary
    offerId OfferInstanceId -- Foreign key to associate with OfferInstance
    totalWalkingTime Int -- Total walking time in seconds
    numberOfTransfers Int -- Number of transfers
    startTime UTCTime -- Trip start time
    startStopName Text -- Name of the starting stop
    lineNumbers [Text] -- List of line numbers used in the trip
    totalTripTime Int -- Total trip time in seconds
    destinationHubName Text -- Name of the destination hub
    profile Text
    deriving Show

TripComputation
    offerId OfferInstanceId
    status Text -- Can be "PENDING", "SUCCESS", "NO_ROUTE_FOUND", "ERROR"
    timestamp UTCTime
    UniqueComputationOffer offerId
    deriving Show
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

upsertFloatingAttr offerId attrName attrValue time =
  case attrValue of
    Just newValue -> do
      _ <-
        upsertBy
          (UniqueOfferName3 offerId attrName)
          ( OfferFloatingAttribute
              time
              offerId
              attrName
              newValue
          )
          [OfferFloatingAttributeValue =. newValue]
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
      let muniArea = details >>= _offerMunicipalityArea
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
      upsertTextAttr offerId "municipality_area" muniArea time

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
      upsertIntAttr
        offerId
        "market"
        ( case details >>= _offerMarket of
            Just MarketPrimary -> Just 0
            Just MarketSecondary -> Just 1
            _ -> Nothing
        )
        time

      upsertFloatingAttr offerId "location_lat" (_latitude <$> (details >>= _offerCoordinates)) time
      upsertFloatingAttr offerId "location_lon" (_longitude <$> (details >>= _offerCoordinates)) time

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
                "municipality_area" ->
                  acc
                    { _offerDetails =
                        Just (details {_offerMunicipalityArea = Just value})
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
                "market" ->
                  acc
                    { _offerDetails =
                        Just
                          ( details
                              { _offerMarket =
                                  case value of
                                    0 -> Just MarketPrimary
                                    1 -> Just MarketSecondary
                                    _ -> Nothing
                              }
                          )
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
    runMigration migrateAll
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

  fetchTripSummaries _ url = runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    offer <- selectFirst [OfferInstanceUrl ==. url] []
    case offer of
      Nothing -> return []
      Just (Entity offerId _) -> do
        tripSummaries <-
          selectList
            [TripSummaryOfferId ==. offerId]
            [ Asc TripSummaryTotalTripTime,
              Asc TripSummaryTotalWalkingTime
            ]
        return $
          map
            ( \(Entity _ ts) ->
                UC.TripSummary
                  { UC.totalWalkingTime = tripSummaryTotalWalkingTime ts,
                    UC.numberOfTransfers = tripSummaryNumberOfTransfers ts,
                    UC.tripStartTime = tripSummaryStartTime ts,
                    UC.tripStartStopName = T.unpack $ tripSummaryStartStopName ts,
                    UC.lineNumbers = map T.unpack $ tripSummaryLineNumbers ts,
                    UC.totalTripTime = tripSummaryTotalTripTime ts,
                    UC.closestHubName = T.unpack $ tripSummaryDestinationHubName ts,
                    UC.profileName = T.unpack $ tripSummaryProfile ts
                  }
            )
            tripSummaries

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

instance LastVisitStorer SQLitePersistence Text where
  storeLastVisit _ user time =
    runSqlite "flatscraper.sqlite" $ do
      runMigration migrateAll
      _ <-
        upsertBy
          (UniqueUserVisit user)
          (Visit user time)
          [VisitTime =. time]
      return ()
  getLastVisit _ user =
    runSqlite "flatscraper.sqlite" $ do
      runMigration migrateAll
      visit <- selectFirst [VisitUser ==. user] []
      case visit of
        Nothing -> return Nothing
        Just (Entity _ (Visit _ time)) -> return $ Just time

instance UC.TripSummaryDataAccess SQLitePersistence OfferInstanceId where
  fetchCoordinatesNeedingComputation _ oneDayAgo = runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll -- Ensures Computation table exists
    latitudes <- selectList [OfferFloatingAttributeName ==. "location_lat", OfferFloatingAttributeCreatedAt >. oneDayAgo] []
    longitudes <- selectList [OfferFloatingAttributeName ==. "location_lon", OfferFloatingAttributeCreatedAt >. oneDayAgo] []

    let latMap = Map.fromList [(offerFloatingAttributeOfferId attr, offerFloatingAttributeValue attr) | Entity _ attr <- latitudes]
    let lonMap = Map.fromList [(offerFloatingAttributeOfferId attr, offerFloatingAttributeValue attr) | Entity _ attr <- longitudes]
    let offerCoordsList = map (\(oid, (lat, lon)) -> (oid, lat, lon)) $ Map.toList $ Map.intersectionWith (,) latMap lonMap

    filterM needsProcessing offerCoordsList
    where
      needsProcessing :: (OfferInstanceId, Double, Double) -> SqlPersistM Bool
      needsProcessing (offerId, _, _) = do
        computation <- selectFirst [TripComputationOfferId ==. offerId] []
        case computation of
          Just (Entity _ c) -> return $ tripComputationStatus c `notElem` ["SUCCESS", "NO_ROUTE_FOUND"] -- Reprocess PENDING or ERROR
          Nothing -> return True -- Not attempted yet

  markComputationAttempt :: SQLitePersistence -> OfferInstanceId -> Text -> UTCTime -> IO ()
  markComputationAttempt _ offerId status time = runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    markComputationStatusDB offerId status time

  storeTripSummary _ offerId tripSummary = runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    insert_ $
      TripSummary
        offerId
        (UC.totalWalkingTime tripSummary)
        (UC.numberOfTransfers tripSummary)
        (UC.tripStartTime tripSummary)
        (T.pack . UC.tripStartStopName $ tripSummary)
        (map T.pack $ UC.lineNumbers tripSummary)
        (UC.totalTripTime tripSummary)
        (T.pack . UC.closestHubName $ tripSummary)
        (T.pack . UC.profileName $ tripSummary)

  markSuccess _ offerId computationTimestamp = runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    markComputationStatusDB offerId "SUCCESS" computationTimestamp

markComputationStatusDB :: OfferInstanceId -> Text -> UTCTime -> SqlPersistM ()
markComputationStatusDB offerId newStatus time = do
  _ <-
    upsertBy
      (UniqueComputationOffer offerId)
      (TripComputation offerId newStatus time)
      [TripComputationStatus =. newStatus, TripComputationTimestamp =. time]
  return ()
