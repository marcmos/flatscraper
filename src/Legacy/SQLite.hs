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
{-# LANGUAGE UndecidableInstances #-}

module Legacy.SQLite
  ( loadPersistedDetails,
    persistOffers,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes)
import Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Legacy.Domain.Offer (Offer (..), OfferExtra)
import Text.Read (readMaybe)

-- TODO rooms
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
  where
    entityQ offer = selectFirst [OfferVisitUrl ==. offerURL offer] []
    entityToRecord offer (Entity _ ent) =
      offer
        { offerVisit = offerVisitScrapeTimestamp ent,
          offerPrice = offerVisitOwnerRentPrice ent,
          offerFiltered = offerVisitFiltered ent,
          offerRentPrice = offerVisitRentPrice ent,
          offerArea = offerVisitArea ent,
          offerRooms = offerVisitRooms ent,
          offerOwnerOffer = offerVisitOwnerOffer ent,
          offerExtras = maybe [] parseExtras (offerVisitExtras ent),
          offerDetailed = True,
          offerDescription = Nothing
        }
    augment offer = maybe offer (entityToRecord offer) <$> entityQ offer

setOfferVisitExtras :: Offer -> OfferVisit -> OfferVisit
setOfferVisitExtras offer offerVisit =
  offerVisit {offerVisitExtras = Just $ T.intercalate "," (T.pack . show <$> offerExtras offer)}

persistOffers :: [Offer] -> IO ()
persistOffers offers = do
  timestamp <- liftIO getCurrentTime
  runSqlite "flatscraper.sqlite" $ do
    runMigration migrateAll
    -- FIXME insertBy
    forM_ offers $ \offer ->
      insertBy . setOfferVisitExtras offer $
        OfferVisit
          { offerVisitScrapeTimestamp = timestamp,
            offerVisitUrl = offerURL offer,
            offerVisitOwnerRentPrice = offerPrice offer,
            offerVisitFiltered = offerFiltered offer,
            offerVisitRentPrice = offerRentPrice offer,
            offerVisitArea = offerArea offer,
            offerVisitRooms = offerRooms offer,
            offerVisitOwnerOffer = offerOwnerOffer offer,
            offerVisitRegion = Nothing,
            offerVisitStreet = Nothing,
            offerVisitExtras = Nothing -- FIXME: filled in by setOfferVisitExtras
          }
