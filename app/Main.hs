{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text())
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Data.Text.Lazy.IO as T (putStr)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Monoid ((<>))
import Control.Monad
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn, hPrint)
import Control.Exception (handle, SomeException)
import Data.CaseInsensitive (mk)

import Text.HTML.Scalpel hiding (scrape, scrapeURL)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import GratkaScraper (gratkaScraper)
import OtodomScraper (otodomScraper)
import OlxScraper (olxScraper)
import GumtreeScraper (gumtreeScraper)
import Newsfeed (renderOfferFeed)
import ScrapePersistence
import OfferFilter (dropBlacklisted)

import Offer

addLegitHeadersNoScam100 :: Request -> IO Request
addLegitHeadersNoScam100 req = return $ req
        { requestHeaders =
                [ (mk $ encodeUtf8 "Accept", encodeUtf8 "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                , (mk $ encodeUtf8 "Accept-Language", encodeUtf8 "pl,en-US;q=0.7,en;q=0.3")
                , (mk $ encodeUtf8 "Cache-Control", encodeUtf8 "no-cache")
                , (mk $ encodeUtf8 "User-Agent", encodeUtf8 "Mozilla/5.0 (X11; Linux x86_64; rv:67.0) Gecko/20100101 Firefox/67.0")
                ]
        }

runScrape :: Config Text -> URL -> Scraper Text a -> IO (Maybe a)
runScrape config url scraper = do
  hPutStrLn stderr $ "Scraping " <> url
  scrapeURLWithConfig config url scraper

scrapeDetails' :: Config Text -> (Offer -> Scraper Text Offer) -> [Offer] -> IO [Offer]
scrapeDetails' config scraper offers = forM offers $ \offer ->
  fromMaybe offer <$>
  if offerDetailed offer
    then return Nothing
    else runScrape config (T.unpack . offerURL $ offer) (scraper offer)

scrape :: OfferScraper -> String -> IO [Offer]
scrape (OfferScraper config template listScraper detailsScraper) =
  scrapeList >=> filterOffers >=> loadPersistedDetails >=> scrapeDetails >=> \offers -> do
    persistOffers offers
    return offers
  where
    scrapeList url = do
      timestamp <- getCurrentTime
      fromMaybe [] <$> runScrape config url (listScraper $ template timestamp)
    scrapeDetails'' = scrapeDetails' config
    scrapeDetails offers = maybe (return offers) (`scrapeDetails''` offers) detailsScraper
    filterOffers = return . dropBlacklisted

safeScrape :: OfferScraper -> String -> IO [Offer]
safeScrape scraper url =
  handle (\e -> do let err = show (e :: SomeException)
                   hPutStrLn stderr err
                   return []) $ scrape scraper url

scrapeURL :: Config Text -> String -> IO [Offer]
scrapeURL config url
  | "gratka.pl"  `isInfixOf` url = safeScrape (gratkaScraper config) url
  | "otodom.pl"  `isInfixOf` url = safeScrape (otodomScraper config) url
  | "olx.pl"     `isInfixOf` url = safeScrape (olxScraper config) url
  | "gumtree.pl" `isInfixOf` url = safeScrape (gumtreeScraper config) url
  | otherwise                = do
      hPutStrLn stderr $ "no scraper for URL " <> url
      return []

main :: IO ()
main = do
  httpManager <- newManager $ tlsManagerSettings { managerModifyRequest = addLegitHeadersNoScam100 }
  let config = Config utf8Decoder (Just httpManager)
  urls <- getArgs
  offers <- concat <$> mapM (scrapeURL config) urls
  hPrint stderr offers
  case renderOfferFeed offers of
     Just x -> T.putStr x
     Nothing -> T.hPutStrLn stderr "Scrap failed"
