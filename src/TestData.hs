{-# LANGUAGE OverloadedStrings          #-}

module TestData where

import Offer

testOffer :: Offer
testOffer = Offer {
      offerVisit = read "2011-11-19 18:28:52.607875 UTC"
    , offerScraperName = "gumtree.pl"
    , offerTitle = "2 POKOJE *** 36M2 *** AZORY *** UMEBLOWANE *** DOBRA KOMUNIKACJA"
    , offerPrice = 1500
    , offerURL = "https://www.gumtree.pl/a-mieszkania-i-domy-do-wynajecia/krakow/2-pokoje-36m2-azory-umeblowane-dobra-komunikacja/1009022892640912716667809"
    , offerDetailed = True
    , offerRooms = Just 2
    , offerRentPrice = Nothing
    , offerLocation = Nothing
    , offerOwnerOffer = Just False
    , offerExtras = []
    }
