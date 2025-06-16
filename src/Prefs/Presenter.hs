{-# LANGUAGE LambdaCase #-}

module Prefs.Presenter (badgeColorMapper) where

import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (cmPricePerMeter),
    HTMLFeedPresenter (HTMLFeedPresenter),
    cmArea,
    defaultColorMapper,
    v1,
  )

badgeColorMapper :: BadgeColorMapper
badgeColorMapper =
  defaultColorMapper
    { cmArea = Just $ \area -> if area >= 70 then "success" else "info",
      cmPricePerMeter = Just $ \case
        x | x < 10000 -> "danger"
        x | x <= 12000 -> "success"
        _ -> "info"
    }
