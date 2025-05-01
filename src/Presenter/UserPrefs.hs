module Presenter.UserPrefs
  ( badgeColorMapper,
    offerFilter,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Offer (OfferView, _offerDetails, _offerDistrict)
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

isPreferredDistrict :: Text -> Bool
isPreferredDistrict input =
  any
    (`T.isSuffixOf` normInput)
    preferredDistricts
  where
    normInput = T.toLower input
    preferredDistricts =
      [ "płaszów",
        "krowodrza",
        "grzegórzki",
        "stare miasto",
        "nowa wieś"
      ]

offerFilter :: OfferView -> Bool
offerFilter o = maybe False isPreferredDistrict district
  where
    district = _offerDetails o >>= _offerDistrict