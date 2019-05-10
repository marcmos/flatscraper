module Main where

import Data.Text as T (Text(), unpack)
import Data.Text.Lazy.IO as TL
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Monoid ((<>))
import Control.Monad

import Text.HTML.Scalpel hiding (scrape)
import GratkaScraper (gratkaScraper)
import OtodomScraper (otodomScraper)
import Newsfeed (renderOfferFeed)
import ScrapePersistence

import Offer

otodomScrapeURL :: String
otodomScrapeURL = "https://www.otodom.pl/wynajem/mieszkanie/krakow/?search%5Bfilter_float_price%3Ato%5D=3000&search%5Bfilter_enum_rooms_num%5D%5B0%5D=3&search%5Bfilter_enum_rooms_num%5D%5B1%5D=4&search%5Bdist%5D=0&search%5Bsubregion_id%5D=410&search%5Bcity_id%5D=38&search%5Border%5D=created_at_first%3Adesc"

gratkaScrapeURL :: String
gratkaScrapeURL = "https://gratka.pl/nieruchomosci/mieszkania/krakow/wynajem?liczba-pokoi:min=3&liczba-pokoi:max=4&cena-calkowita:max=3000&sort=newest"

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

saveNewsfeed :: IO ()
saveNewsfeed = do
  otodomOffers <- scrape otodomScraper otodomScrapeURL
  gratkaOffers <- scrape gratkaScraper gratkaScrapeURL
  case renderOfferFeed $ otodomOffers ++ gratkaOffers of
    Just x -> TL.writeFile "newsfeed.xml" x
    Nothing -> Prelude.putStrLn "Scrap failed"

main :: IO ()
main = saveNewsfeed
