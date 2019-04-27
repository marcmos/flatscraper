module Main where

import Data.Text.Lazy as T (Text())
import Data.Text.Lazy.IO as T
import Data.List (concat)
import Data.Maybe (catMaybes)

import GratkaScraper
import OtodomScraper
import Newsfeed (renderOfferFeed)

offerFeed :: IO (Maybe Text)
offerFeed = do
  otodomOffers <- flatScrapOtodomOffers "https://www.otodom.pl/wynajem/mieszkanie/krakow/?search%5Bfilter_float_price%3Ato%5D=3000&search%5Bfilter_enum_rooms_num%5D%5B0%5D=3&search%5Bfilter_enum_rooms_num%5D%5B1%5D=4&search%5Bdist%5D=0&search%5Bsubregion_id%5D=410&search%5Bcity_id%5D=38&search%5Border%5D=created_at_first%3Adesc"
  gratkaOffers <- scrapGratkaOffers "https://gratka.pl/nieruchomosci/mieszkania/krakow/wynajem?liczba-pokoi:min=3&liczba-pokoi:max=4&cena-calkowita:max=3000&sort=newest"
  return $ renderOfferFeed (concat $ catMaybes [otodomOffers, gratkaOffers])

saveNewsfeed :: IO ()
saveNewsfeed = do
  feed <- offerFeed
  case feed of
    Just x -> T.writeFile "/tmp/newsfeed.xml" x
    Nothing -> Prelude.putStrLn "Scrap failed"

main :: IO ()
main = saveNewsfeed
