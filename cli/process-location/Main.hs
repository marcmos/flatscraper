module Main where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import DataAccess.Mobroute (MobrouteProvider (MobrouteProvider))
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import UseCase.GenerateTripSummary
  ( CityTransport (..),
    generateAndStoreTripSummaries,
  )

main :: IO ()
main = do
  let sqlite = SQLitePersistence
  time <- startTime'
  generateAndStoreTripSummaries sqlite mobroute krakow time

mobroute :: MobrouteProvider
mobroute =
  MobrouteProvider
    "/home/mmos/src/mobroute/mobroute"
    feedIds
    defaultTimeLocale

startTime' :: IO UTCTime
startTime' = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2025-05-15T06:00:00Z"

feedIds :: [Int]
feedIds =
  [ 1326,
    1270,
    2598
  ]

hubs :: [(String, (Double, Double))]
hubs =
  [ ("Rondo Mogilskie", (50.065759, 19.959864)),
    ("Rondo Grzegorzeckie", (50.057888, 19.960296)),
    -- ("Rondo Matecznego", (50.03640915612283, 19.940682902040095)),
    ("Dworzec Towarowy", (50.07565532179814, 19.940292936723765)),
    ("Stradom", (50.051741810252906, 19.94163361052291)),
    ("Starowiślna", (50.05639992924672, 19.94544393191966)),
    ("Teatr Bagatela", (50.06350324938812, 19.932918547197723))
  ]

krakow :: CityTransport
krakow = CityTransport hubs
