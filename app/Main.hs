module Main where

import Data.Text as T (Text(), unpack)
import Data.Text.Lazy.IO as TL
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Monoid ((<>))
import Control.Monad
import Data.List (isInfixOf)
import System.Environment (getArgs)
import Control.Exception (handle, SomeException)

import Text.HTML.Scalpel hiding (scrape, scrapeURL)
import GratkaScraper (gratkaScraper)
import OtodomScraper (otodomScraper)
import Newsfeed (renderOfferFeed)
import ScrapePersistence

import Offer

runScrape :: URL -> Scraper Text a -> IO (Maybe a)
runScrape url scraper = do
  Prelude.putStrLn $ "Scraping " <> url
  scrapeURLWithConfig config url scraper
  where config = Config utf8Decoder Nothing

scrapeDetails' :: (Offer -> Scraper T.Text Offer) -> [Offer] -> IO [Offer]
scrapeDetails' scraper offers = forM offers $ \offer ->
  fromMaybe offer <$>
  if offerDetailed offer
    then return Nothing
    else runScrape (unpack . offerURL $ offer) (scraper offer)

scrape :: OfferScraper -> String -> IO [Offer]
scrape offerScraper =
  scrapeList >=> loadPersistedDetails >=> scrapeDetails >=> \offers -> do
    persistOffers offers
    return offers
  where
    listScraper = offerListScraper offerScraper
    detailsScraper = offerDetailsScraper offerScraper
    scrapeList url = do
      timestamp <- getCurrentTime
      fromMaybe [] <$> runScrape url (listScraper timestamp)
    scrapeDetails offers = maybe (return offers) (`scrapeDetails'` offers) detailsScraper

safeScrape :: OfferScraper -> String -> IO [Offer]
safeScrape scraper url =
  handle (\e -> do let err = show (e :: SomeException)
                   Prelude.putStrLn err
                   return []) $ scrape scraper url

scrapeURL :: String -> IO [Offer]
scrapeURL url
  | "gratka.pl" `isInfixOf` url = safeScrape gratkaScraper url
  | "otodom.pl" `isInfixOf` url = safeScrape otodomScraper url
  | otherwise                = do
      Prelude.putStrLn $ "no scraper for URL " ++ url
      return []

main :: IO ()
main = do
  urls <- getArgs
  offers <- concat <$> mapM scrapeURL urls
  case renderOfferFeed offers of
     Just x -> TL.writeFile "newsfeed.xml" x
     Nothing -> Prelude.putStrLn "Scrap failed"
