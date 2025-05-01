module Presenter.UserPrefs (badgeColorMapper) where

import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (cmPricePerMeter),
    cmArea,
    defaultColorMapper,
  )

badgeColorMapper :: BadgeColorMapper
badgeColorMapper =
  defaultColorMapper
    { cmArea = Just $ \area -> if area >= 70 then "success" else "info",
      cmPricePerMeter = Just $ \ppm -> if ppm <= 12000 then "success" else "info"
    }