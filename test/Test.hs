module Main where

import Scraper.Dictionary (parseLocationText)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseLocationText" $ do
    it "parses location text correctly" $ do
      parseLocationText "Kraków, Prądnik Biały, Henryka Pachońskiego"
        `shouldBe` (Just "Henryka Pachońskiego", Nothing, Just "Prądnik Biały")

    it "parses location text correctly" $ do
      parseLocationText
        "Kraków, Kraków-Krowodrza, Prądnik Biały, Władysława Łokietka"
        `shouldBe` ( Just "Władysława Łokietka",
                     Just "Prądnik Biały",
                     Just "Krowodrza"
                   )

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Kraków-Nowa Huta, Mistrzejowice"
        `shouldBe` (Nothing, Just "Mistrzejowice", Just "Nowa Huta")

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Ruczaj, Prof. Władysława Konopczyńskiego"
        `shouldBe` ( Just "Prof. Władysława Konopczyńskiego",
                     Just "Ruczaj",
                     Nothing
                   )

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Kraków-Krowodrza, Bielany"
        `shouldBe` (Nothing, Just "Bielany", Just "Krowodrza")

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Kraków-Krowodrza, Sosnowiecka"
        `shouldBe` (Just "Sosnowiecka", Nothing, Just "Krowodrza")

    it "parses location text correctly" $ do
      parseLocationText "Kraków M., Kraków, Bronowice, Armii Krajowej"
        `shouldBe` (Just "Armii Krajowej", Nothing, Just "Bronowice")

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Borek Fałęcki, Kraków"
        `shouldBe` (Nothing, Just "Borek Fałęcki", Nothing)

    it "parses location text correctly" $ do
      parseLocationText "Kraków, Dębniki, Os. Ruczaj, Zalesie"
        `shouldBe` (Just "Zalesie", Just "Ruczaj", Just "Dębniki")

    it "parses case for nieruchomosci-online.pl list correctly" $ do
      parseLocationText "Grzegórzki, Kraków"
        `shouldBe` ( Nothing,
                     Nothing,
                     Just "Grzegórzki"
                   )
