module Presenter.CLIDigestPresenter (CLIPresenter (CLIPresenter)) where

import Control.Monad (forM_)
import UseCase.DigestGenerator (DigestPresenter (present), OfferDigest (OfferDigest), OfferDigestItem (_itemURL))

data CLIPresenter = CLIPresenter Int Double

instance DigestPresenter CLIPresenter where
  present (CLIPresenter i d) (OfferDigest offers) = do
    forM_ offers (print . _itemURL)
