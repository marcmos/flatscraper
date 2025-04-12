# flatscraper

There are two types of generators:
1. ListLoader -- generates new offers
2. DetailsLoader -- modifies offer, possibly adding more details.

Implementations:
1. BasicScraperLoader -- returns offers from the scraped site, single-page fetch
2. ScrapeDetailsLoader -- augments offers by scraping pages
2. PersistedDetailsLoader -- augments offers by loading from database