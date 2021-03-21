{-# LANGUAGE OverloadedStrings          #-}

module TestData where

import Data.Text
import Offer
import GumtreeScraper
import Text.HTML.Scalpel
import WordUtils (possibleRentPrices, possibleRentPrice)

testOffer :: Offer
testOffer = Offer {
      offerVisit = read "2011-11-19 18:28:52.607875 UTC"
    , offerScraperName = "gumtree.pl"
    , offerTitle = "2 POKOJE *** 36M2 *** AZORY *** UMEBLOWANE *** DOBRA KOMUNIKACJA"
    , offerPrice = 1500
    , offerURL = "https://www.gumtree.pl/a-mieszkania-i-domy-do-wynajecia/krakow/2-pokoje-36m2-azory-umeblowane-dobra-komunikacja/1009022892640912716667809"
    , offerDetailed = True
    , offerArea = Just 36
    , offerRooms = Just 2
    , offerRentPrice = Nothing
    , offerLocation = Nothing
    , offerOwnerOffer = Just False
    , offerExtras = []
    , offerFiltered = Nothing
    }

testURLGumtree = "https://www.gumtree.pl/a-mieszkania-i-domy-do-wynajecia/krakow/wynajme-2+pokojowe-mieszkanie-na-armii-krajowej-w-krakowie/1007741585170910536600309"

exampleDescription :: Text
exampleDescription = "Do wynaj\281cia nowe luksusowe mieszkanie na 2 pi\281trze przy ul. Armii Krajowej w Krakowie (obok miasteczka studenckiego). Mieszkanie jest w pe\322ni umeblowane i w pe\322ni wyposa\380one w sprz\281ty AGD (pralka, lod\243wka, p\322yta indukcyjna). Budynek jest pi\281knie po\322o\380ony zda\322a od drogi g\322\243wnej (200m), obok jest kompleks z basenami. Okolica jest bardzo cicha. Wychodz\261c z budynku jest od razu przystanek autobusowy. Przy budynku jest powierzchnia dla rower\243w oraz budynek jest obj\281ty monitoringiem.  \r\nBardzo dobra lokalizacja i centrum miasta. \r\n\r\nKoszt: 2000z\322/miesi\261c plus czynsz administracyjny 500 z\322, ca\322kowity koszt wynaj\281cia to oko\322o 2500 z\322.\r\nW cenie:\r\n-ogrzewanie\r\n-woda ciep\322a i zimna\r\n-pr\261d\r\n-wyw\243z \347mieci\r\n\r\nBrak dodatkowych op\322at.\r\n\r\nBez po\347rednik\243w.\r\n\r\nMieszkanie \347wie\380o po remoncie, przygotowane pod wynajem. Ogrzewanie i woda ciep\322a miejskie, budynek ocieplony, okna wymienione. Akceptujemy zwierz\281ta.\r\n\r\nWymagana kaucja w wysoko\347ci jednomiesi\281cznego czynszu\r\nkontakt: +48 604 509 020, 600 709 062 \r\n"

testRunGumtreeDetails url =
  scrapeURL url (detailedOfferScraper testOffer)

testRentPrices url =
  (\x -> (\z -> do
    area <- offerArea z
    desc <- offerDescription z
    return $ possibleRentPrices area desc) <$> x) <$> testRunGumtreeDetails url

testDescription :: Text
testDescription = "Do wynaj\281cia nowe mieszkanie z balkonem w cichej okolicy. Mieszkanie wyposa\380one w wysokim standardzie.LOKALIZACJA:Budynek\r\n po\322o\380ony przy ulicy Zakrz\243wieckiej w nowoczesnym apartamentowcu, \r\npo\322o\380onego na terenie osiedla o kontrolowanym dost\281pie. Spokojna cicha okolica.\346wietna \r\nlokalizacja, kt\243ra umo\380liwia szybki dojazd do Centrum miasta (do 10-15 \r\nminut). Tu\380 obok park biznesowy Porto Office, HSBC, Shell, Motorola - \r\noraz Kampusu UJ. W pobli\380u bogata infrastruktura handlowo \8211 us\322ugowa, \r\nsklepy, restauracje, centrum handlowe Bonarka, Tesco, Zakopianka, Lidl. \r\nBardzo szybki dojazd do autostrady A4 i dr\243g wylotowych z Krakowa.Dla\r\n aktywnych mo\380liwo\347\263 skorzystania z: Zalew Zakrz\243wek, Krakowski Park \r\nLinowy, Ska\322ki Twardowskiego, Biela\324sko-Tyniecki Park Krajobrazowy oraz \r\ntrasa rowerowa przy Wi\347le.MIESZKANIEMieszkanie 2-pokojowe z antresol\261 o powierzchni 48 m2 znajduje si\281 na 3 pi\281trzei sk\322ada si\281 z:- aneks kuchenny: lod\243wka, zmywarka, piekarnik, p\322yta grzewcza indukcyjna, czajnik, okap, lada kuchenna i Hokery- salon: rozk\322adana sofa, szafka RTV, telewizor, stolik kawowy,- sypialni: \322\243\380ko podw\243jne, szafa.- antresola: \322\243\380ko podw\243jne- \322azienki: wanna, WC, umywalka, szafki, pralka.- przedpokoju: szafa.- balkonu z pi\281knym widokiemISTNIEJE MO\379LIWO\346\262 DOPOSA\379ENIA MIESZKANIA NA \379YCZENIE NAJEMCYCENA:2000 PLN - czynsz najmu450 PLN - czynsz administracyjnyMedia wg zu\380yciaWYNAGRODZENIE DLA BIURA NIERUCHOMO\346CI POKRYWA W\321A\346CICIEL"