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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataAccess.SQLite
  ( SQLitePersistence (SQLitePersistence),
    SQLiteOfferQuery (SQLiteOfferQuery),
  )
where

import Control.Monad (filterM, forM, forM_, void, (<=<))
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.Functor
import Data.Int (Int64)
import Data.List (findIndex, nub, sortBy)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (concat, intercalate, pack, unpack)
import Data.Time (UTCTime, addDays, addUTCTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.Persist (Entity (Entity), Key, PersistField (fromPersistValue), PersistValue (PersistInt64), SelectOpt (Asc, Desc, LimitTo), SqlType (SqlInt64), getEntity, insert, insertUnique, insert_, selectFirst, selectList, toPersistValue, upsertBy, (=.), (==.), (>.))
import Database.Persist.Sql (PersistFieldSql (sqlType), Single (Single, unSingle), SqlPersistM, fromSqlKey, rawSql, runMigration, toSqlKey)
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
    OfferView (OfferView, _offerArea, _offerLatestPrice, _offerTitle, _offerURL),
    emptyDetails,
    _offerDetails,
  )
import qualified Domain.Offer as DO
import UseCase.EmergingOffer (EmergingOfferSelector (..))
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

instance PersistField DO.OfferInstanceId where
  toPersistValue (DO.OfferInstanceId i) = PersistInt64 i
  fromPersistValue pv = DO.OfferInstanceId <$> fromPersistValue pv

instance PersistFieldSql DO.OfferInstanceId where
  sqlType _ = SqlInt64

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

OfferLabel
  createdAt UTCTime
  offerId OfferInstanceId
  label Text
  UniqueOfferLabel offerId label
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

PublishedOffer
    offerId DO.OfferInstanceId
    createdAt UTCTime
    feedId Text
    UniquePublishedOffer offerId feedId
    score Double Maybe
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

persistOffers :: [(OfferView, [Text])] -> IO ()
persistOffers offersWithLabels = do
  time <- getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    forM_ offersWithLabels $
      \( OfferView _ url price area title details,
         labels
         ) -> do
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

          -- Store the provided labels
          forM_ labels $ \label ->
            void $ insertUnique $ OfferLabel time offerId label

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

toOfferView :: DO.OfferInstanceId -> OfferInstance -> OfferView
toOfferView
  offerId
  ( OfferInstance
      { offerInstanceUrl = url,
        offerInstancePrice = price,
        offerInstanceArea = area,
        offerInstanceTitle = title
      }
    ) =
    OfferView (Just offerId) url price area title (Just emptyDetails)

-- loadAttributes :: Entity OfferInstance -> OfferView
loadAttributes (Entity offerId offerInstance) = do
  let offer = toOfferView (DO.OfferInstanceId $ fromSqlKey offerId) offerInstance
  textAttrs <- selectList [OfferTextAttributeOfferId ==. offerId] []
  let offer' = loadTextAttrs offer textAttrs
  intAttrs <- selectList [OfferIntegralAttributeOfferId ==. offerId] []
  let offer'' = loadIntegralAttrs offer' intAttrs
  return offer''

loadAttributes' offerId offer = do
  textAttrs <- selectList [OfferTextAttributeOfferId ==. offerId] []
  let offer' = loadTextAttrs offer textAttrs
  intAttrs <- selectList [OfferIntegralAttributeOfferId ==. offerId] []
  let offer'' = loadIntegralAttrs offer' intAttrs
  return offer''

getCreatedAfter :: UTCTime -> Int -> IO [OfferView]
getCreatedAfter timestamp limit =
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    let rawQuery =
          "SELECT offer_instance.id, url, title, area, price \
          \FROM offer_instance \
          \LEFT JOIN scoreboard_boosted \
          \ON offer_instance.id = scoreboard_boosted.id \
          \WHERE datetime(offer_instance.created_at) >= datetime(?) \
          \ORDER BY score DESC, price / area ASC \
          \LIMIT ?"
    row <-
      rawSql rawQuery [toPersistValue timestamp, toPersistValue limit] ::
        SqlPersistM
          [ ( Single OfferInstanceId,
              Single Text,
              Single Text,
              Single Double,
              Single Int
            )
          ]

    let instances =
          map
            ( \(Single offerId, Single url, Single title, Single area, Single price) ->
                ( offerId,
                  OfferView
                    { _offerURL = url,
                      _offerLatestPrice = price,
                      _offerArea = area,
                      _offerTitle = title,
                      _offerDetails = Nothing
                    }
                )
            )
            row

    mapM (uncurry loadAttributes') instances

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

data SQLiteOfferQuery = SQLiteOfferQuery
  { persistence :: SQLitePersistence,
    query :: Text
  }

instance QueryAccess SQLiteOfferQuery where
  getOffersCreatedAfter (SQLiteOfferQuery _ query) timestamp = do
    runSqlite "flatscraper.sqlite" $
      ( ( do
            runMigration migrateAll
            rawQuery <-
              rawSql query [] ::
                SqlPersistM [(Single OfferInstanceId, Single Double)]
            forM rawQuery $ \(Single offerId, Single _) -> do
              offerEntity <- selectFirst [OfferInstanceId ==. offerId] []
              case offerEntity of
                Nothing -> return Nothing
                Just entity -> do
                  offer <- loadAttributes entity
                  return $ Just offer
        )
          <&> catMaybes
      )

  fetchTripSummaries (SQLiteOfferQuery persistence _) url = do
    fetchTripSummaries persistence url

-- RSS stuff
instance EmergingOfferSelector SqlPersistM where
  executeOfferSourceQuery :: Text -> [PersistValue] -> SqlPersistM [DO.OfferInstanceId]
  executeOfferSourceQuery sqlQuery params = do
    results <- rawSql sqlQuery params :: SqlPersistM [Single Int64]
    return $ map (DO.OfferInstanceId . unSingle) results

  filterSortAndLoadOffers ::
    Text ->
    [DO.OfferInstanceId] ->
    SqlPersistM [DO.OfferView]
  filterSortAndLoadOffers feedId domainCandidateIds = do
    if null domainCandidateIds
      then return []
      else do
        -- 1. Get all OfferInstanceIds that are already in the RSS history (the exclusion list)
        rssHistoryExclusionIds_singles <-
          rawSql
            "SELECT DISTINCT offer_id FROM published_offer WHERE feed_id = ?"
            [toPersistValue feedId] :: -- rssHistoryFeedId from UseCase.EmergingOffer
            SqlPersistM [Single DO.OfferInstanceId]
        let exclusionSet = Set.fromList $ map unSingle rssHistoryExclusionIds_singles

        -- 2. Iterate through the original domainCandidateIds, preserving order,
        --    and load those not in the exclusion set.
        let loadIfNotExcluded :: DO.OfferInstanceId -> SqlPersistM (Maybe DO.OfferView)
            loadIfNotExcluded domainId =
              if Set.member domainId exclusionSet
                then return Nothing -- It's in RSS history, so exclude it
                else do
                  -- Load the OfferView for this ID
                  let persistentKey = toSqlKey (DO.unOfferInstanceId domainId) :: Key OfferInstance
                  mEntity <- getEntity persistentKey
                  case mEntity of
                    Just entity -> Just <$> loadAttributes entity -- Assuming loadAttributes is your function
                    Nothing -> return Nothing -- Offer instance not found, treat as excluded

        -- Use forM to process in order and then catMaybes to remove Nothings
        catMaybes <$> forM domainCandidateIds loadIfNotExcluded

  markAsRssPublished :: DO.OfferInstanceId -> UTCTime -> Text -> Text -> SqlPersistM ()
  markAsRssPublished domainOfferId publishTime globalScoreTableName feedId = do
    -- 1. Get the 'global' score of the current offer being published.
    let currentGlobalScoreQuery = "SELECT score FROM \"" <> globalScoreTableName <> "\" WHERE id = ? LIMIT 1"
    currentOfferGlobalScoreEntries <-
      rawSql currentGlobalScoreQuery [toPersistValue (DO.unOfferInstanceId domainOfferId)] ::
        SqlPersistM [Single (Maybe Double)] -- Specify type for score
    let currentOfferGlobalScoreM :: Maybe Double -- Explicit type for clarity
        currentOfferGlobalScoreM = case currentOfferGlobalScoreEntries of
          [(Single scoreValue)] -> scoreValue
          _ -> Nothing

    -- 2. Get the OfferInstanceIds of the last 9 distinct offers already published to RSS.
    last9RssOfferInstanceIds_singles <-
      rawSql
        "SELECT DISTINCT offer_id FROM published_offer WHERE feed_id = ? AND offer_id != ? ORDER BY created_at DESC LIMIT 9"
        [toPersistValue feedId, toPersistValue domainOfferId] ::
        SqlPersistM [Single DO.OfferInstanceId]
    let last9RssOfferInstanceIds = map unSingle last9RssOfferInstanceIds_singles

    -- 3. For these 9 OfferInstanceIds, fetch their 'global' scores from the specified globalScoreTableName.
    let prevGlobalScoreQuery = "SELECT score FROM \"" <> globalScoreTableName <> "\" WHERE id = ? LIMIT 1"
    previousOffersGlobalScores <- forM last9RssOfferInstanceIds $ \prevOfferId -> do
      globalScoreEntries <-
        rawSql prevGlobalScoreQuery [toPersistValue (DO.unOfferInstanceId prevOfferId)] ::
          SqlPersistM [Single (Maybe Double)] -- Specify type for score
      case globalScoreEntries of
        [(Single scoreValue)] -> return (prevOfferId, scoreValue :: Maybe Double)
        _ -> return (prevOfferId, Nothing :: Maybe Double)

    -- 4. Combine current offer (with its global score) with the previous offers (with their global scores).
    let currentOfferForRanking = (domainOfferId, currentOfferGlobalScoreM :: Maybe Double)
    let allOffersForRanking :: [(DO.OfferInstanceId, Maybe Double)] -- Explicit type
        allOffersForRanking = filter (\(_, scoreM) -> scoreM /= (Nothing :: Maybe Double)) (currentOfferForRanking : previousOffersGlobalScores)

    let calculatedLocalPercentageScoreM :: Maybe Double -- Explicit type
        calculatedLocalPercentageScoreM =
          case currentOfferGlobalScoreM of
            Nothing -> Nothing
            Just currentGlobalScore ->
              -- currentGlobalScore is Double here
              if null allOffersForRanking || (length allOffersForRanking == 1 && fst (head allOffersForRanking) == domainOfferId)
                then Just 100.0
                else
                  let sortedGlobalScores :: [Maybe Double] -- Explicit type
                      sortedGlobalScores = sortBy (compare `on` Down) $ map snd allOffersForRanking
                      mRank0 = L.findIndex (== Just currentGlobalScore) sortedGlobalScores -- L.findIndex or findIndex
                      totalRankedItems = length sortedGlobalScores
                   in case mRank0 of
                        Nothing -> Nothing -- Should not happen if currentGlobalScore was in allOffersForRanking
                        Just rank0 ->
                          if totalRankedItems == 0
                            then Nothing -- Should not happen
                            else
                              let rank1 = rank0 + 1
                                  localScoreValue = (fromIntegral (totalRankedItems - rank1 + 1) / fromIntegral totalRankedItems) * 100.0
                               in Just localScoreValue

    -- 5. Insert the new RSS history entry.
    _ <-
      insert_ $
        PublishedOffer
          { publishedOfferOfferId = domainOfferId,
            publishedOfferCreatedAt = publishTime,
            publishedOfferFeedId = feedId,
            publishedOfferScore = calculatedLocalPercentageScoreM -- This is Maybe Double
          }
    return ()