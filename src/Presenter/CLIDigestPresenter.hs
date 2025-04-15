module Presenter.CLIDigestPresenter (CLIPresenter (CLIPresenter)) where

import UseCase.DigestGenerator (DigestPresenter (present))
import UseCase.Offer ()

data CLIPresenter = CLIPresenter Int Double

instance DigestPresenter CLIPresenter where
  present (CLIPresenter i d) resultFeed = do
    print resultFeed
