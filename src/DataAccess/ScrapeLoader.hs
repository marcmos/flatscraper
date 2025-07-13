module DataAccess.ScrapeLoader
  ( ScraperPack (ScraperPack),
    WebScraper (WebScraper),
    prefixWebScraper,
    ScrapeSource (WebSource, FileSource),
    WebScrapers (WebScrapers),
    ScrapeAction (ScrapeDetails, ScrapeAndFollow),
    FollowUpRule (FollowUpRule),
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

data ScrapeAction
  = ScrapeDetails DetailsScraper
  | ScrapeAndFollow (OfferView -> Scraper Text (OfferView, [URL])) [FollowUpRule]

data FollowUpRule = FollowUpRule (URL -> Bool) ScrapeAction

data ScraperPack = ScraperPack ListScraper (Maybe ScrapeAction)

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
  seedOffers (FileSource (ScraperPack listScraper maybeAction) path) = do
    htmlContent <- T.readFile path
    let offers = scrapeStringLike htmlContent listScraper
    case offers of
      Just (x : xs) -> return (x : xs)
      _ ->
        maybe
          (return [])
          ( \dSc ->
              return . maybeToList $ scrapeStringLike htmlContent (dSc Nothing)
          )
          ( case maybeAction of
              Just (ScrapeDetails detailsScraper) -> Just detailsScraper
              _ -> Nothing
          )
  seedOffers (WebSource (WebScrapers config scrapers) url) = do
    case findPack url scrapers of
      Just (WebScraper (ScraperPack listScraper maybeAction) _) -> do
        offerList <- scrapeLog "offers" url listScraper
        case offerList of
          Just offers -> return offers
          Nothing -> case maybeAction of
            Just (ScrapeDetails detailsScraper) ->
              maybeToList <$> scrapeLog "fallback" url (detailsScraper Nothing)
            _ -> return []
      Nothing -> return []
    where
      scrape = maybe scrapeURL scrapeURLWithConfig config
      scrapeLog str scrapeUrl scraper = do
        putStrLn $ "Scrapping " <> str <> " " <> scrapeUrl
        scrape scrapeUrl scraper

instance OfferDetailsLoader WebScrapers where
  loadDetails (WebScrapers config scrapers) ov =
    case findPack url scrapers of
      Just (WebScraper (ScraperPack _ (Just action)) _) ->
        processScrapeAction action ov url
      _ -> return ov
    where
      url = unpack . _offerURL $ ov
      scrape = maybe scrapeURL scrapeURLWithConfig config
      scrapeLog str scrapeUrl scraper = do
        putStrLn $ "Scrapping " <> str <> " " <> scrapeUrl
        scrape scrapeUrl scraper

      processScrapeAction :: ScrapeAction -> OfferView -> URL -> IO OfferView
      processScrapeAction (ScrapeDetails detailsScraper) offer currentUrl = do
        maybeOffer <- scrapeLog "details" currentUrl (detailsScraper (Just offer))
        return $ fromMaybe offer maybeOffer
      processScrapeAction (ScrapeAndFollow scraper rules) offer currentUrl = do
        scrapeResult <- scrapeLog "and follow" currentUrl (scraper offer)
        case scrapeResult of
          Just (updatedOffer, followUpUrls) ->
            foldl (processFollowUpRule rules) (return updatedOffer) followUpUrls
          Nothing -> return offer

      processFollowUpRule :: [FollowUpRule] -> IO OfferView -> URL -> IO OfferView
      processFollowUpRule rules accIO followUpUrl = do
        acc <- accIO
        case find (\(FollowUpRule p _) -> p followUpUrl) rules of
          Just (FollowUpRule _ action) -> processScrapeAction action acc followUpUrl
          Nothing -> return acc
