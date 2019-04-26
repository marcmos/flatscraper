module Main where

import GratkaScraper
import OtodomScraper

main :: IO ()
main = scrapOtodomOffers "https://www.otodom.pl/wynajem/mieszkanie/krakow/krowodrza/?search%5Bfilter_float_price%3Ato%5D=2000&search%5Bfilter_enum_rooms_num%5D%5B0%5D=1&search%5Bfilter_enum_rooms_num%5D%5B1%5D=2&search%5Bdescription%5D=1&search%5Bdist%5D=0&search%5Bdistrict_id%5D=342&search%5Bsubregion_id%5D=410&search%5Bcity_id%5D=38"
