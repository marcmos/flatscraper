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
* rename Persistence do DataAccess
* set OverloadedString project-wide
* run SQL migrations once
* keep SQLite filename in instance field

dodać scraper olx
dodać details scraper selector
wygenerować rss feed olx (tylko takie dane, jakie mam)
dodać nowego maina do scrape'owania, który bierze linki z args
wygenerować digest cli z hardcode'owaną datą północ
dodać pole o typie ogłoszenia