# flatscraper

There are three types of generators:
1. ListLoader -- generates new offers.
* ScrapeListLoader
* SQLitePersistence
2. DetailsLoader -- modifies offer, possibly adding more details.
* ScrapeDetailsLoader
* SQLitePersistence

There are two OfferStorers:
1. NoOpStorer
2. SQLitePersistence

TODO:
* set OverloadedString project-wide
* run SQL migrations once
* keep SQLite filename in instance field
* strip Scraper suffix from module names

wygenerować digest cli z hardcode'owaną datą północ
dodać pole o typie ogłoszenia
podzielić dzielnice na kategorie, od najlepszych do najgorszych
do prezentacji: dostępność (pierwsze piętro albo winda), czy do remontu, czy blisko przystanek
    tramwajowy/autobusowy, czy jest gaz, ilość pięter, parking, klimatyzacja
trackowanie liczby wyświetleń