FROM haskell:8

RUN apt-get update -y && apt-get install -y -qq libcurl4-openssl-dev

WORKDIR /usr/local/bin

ADD ./out/flatscraper .
ENV GRATKA_URL "https://gratka.pl/nieruchomosci/mieszkania/krakow/wynajem?liczba-pokoi:min=3&liczba-pokoi:max=4&cena-calkowita:max=3000&sort=newest"
ENV OTODOM_URL "https://www.otodom.pl/wynajem/mieszkanie/krakow/?search%5Bfilter_float_price%3Ato%5D=3000&search%5Bfilter_enum_rooms_num%5D%5B0%5D=3&search%5Bfilter_enum_rooms_num%5D%5B1%5D=4&search%5Bdist%5D=0&search%5Bsubregion_id%5D=410&search%5Bcity_id%5D=38&search%5Border%5D=created_at_first%3Adesc"
RUN mkdir res
CMD ["/bin/bash", "-c", "cd res; while true ; do sleep 2; ../flatscraper $GRATKA_URL $OTODOM_URL; done"]