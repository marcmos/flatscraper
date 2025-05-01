module DataAccess.ScrapeLoader
  ( ScraperPack (ScraperPack),
    WebScraper (WebScraper),
    prefixWebScraper,
    ScrapeSource (WebSource, FileSource),
    WebScrapers (WebScrapers),
  )
where

import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (readFile)
import Domain.Offer (OfferView (_offerURL))
import Text.HTML.Scalpel
  ( Config,
    Scraper,
    URL,
    scrapeStringLike,
    scrapeURL,
    scrapeURLWithConfig,
  )
import UseCase.Offer (OfferSeeder (seedOffers))
import UseCase.ScrapePersister (OfferDetailsLoader (loadDetails))

type ListScraper = Scraper Text [OfferView]

type DetailsScraper = Maybe OfferView -> Scraper Text OfferView

data ScraperPack = ScraperPack ListScraper (Maybe DetailsScraper)

data WebScraper = WebScraper ScraperPack (URL -> Bool)

data WebScrapers = WebScrapers (Maybe (Config Text)) [WebScraper]

prefixWebScraper :: URL -> ScraperPack -> WebScraper
prefixWebScraper url pack = WebScraper pack (isPrefixOf url)

data ScrapeSource
  = FileSource ScraperPack FilePath
  | WebSource WebScrapers URL

findPack :: (Foldable t) => URL -> t WebScraper -> Maybe WebScraper
findPack u = find (\(WebScraper _ p) -> p u)

instance OfferSeeder ScrapeSource where
  seedOffers (FileSource (ScraperPack listScraper detailsScraper) path) = do
    htmlContent <- T.readFile path
    let offers = scrapeStringLike htmlContent listScraper
    case offers of
      Just (x : xs) -> return (x : xs)
      _ ->
        maybe
          (return [])
          ( \dSc ->
              return . maybeToList $
                scrapeStringLike
                  htmlContent
                  (dSc Nothing)
          )
          detailsScraper
  seedOffers (WebSource (WebScrapers config scrapers) url) = do
    case findPack url scrapers of
      Just (WebScraper (ScraperPack listScraper offerScraper) _) -> do
        offerList <- scrapeLog "offers" url listScraper
        case offerList of
          Just offers -> return offers
          Nothing ->
            maybe
              (return [])
              ( \dSc ->
                  maybeToList
                    <$> scrapeLog
                      "fallback"
                      url
                      (dSc Nothing)
              )
              offerScraper
      Nothing -> return []
    where
      scrape = do
        maybe scrapeURL scrapeURLWithConfig config
      scrapeLog str scrapeUrl scraper = do
        putStrLn $ "Scrapping " <> str <> " " <> scrapeUrl
        scrape scrapeUrl scraper

instance OfferDetailsLoader WebScrapers where
  loadDetails (WebScrapers config scrapers) ov = do
    let pack = findPack url scrapers
    case pack of
      Just (WebScraper (ScraperPack _ (Just detailsScraper)) _) -> do
        newOffer <- scrapeLog "details" url (detailsScraper (Just ov))
        return $ fromMaybe ov newOffer
      _ -> return ov
    where
      url = unpack . _offerURL $ ov
      scrape = do
        maybe scrapeURL scrapeURLWithConfig config
      scrapeLog str scrapeUrl scraper = do
        putStrLn $ "Scrapping " <> str <> " " <> scrapeUrl
        scrape scrapeUrl scraper
