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