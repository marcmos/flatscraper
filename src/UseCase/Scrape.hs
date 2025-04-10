module UseCase.Scrape () where
import UseCase.Offer (StubOffer, DetailedOffer)

class OfferDetailStorer ods where
    storeDetails :: ods -> StubOffer -> IO DetailedOffer