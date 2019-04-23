module Main where

import GratkaScraper

main :: IO ()
main = scrapGratkaOffers "https://gratka.pl/nieruchomosci/mieszkania/krakow/krowodrza/wynajem?liczba-pokoi:min=3&liczba-pokoi:max=5&cena-calkowita:max=3000&sort=newest"
