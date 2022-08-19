# Analiza vpliva makroekonomskih kazalcev na trajanje bolniškega staleža v Sloveniji


## Tematika

Za projektno nalogo bom analiziral bolniški stalež v sloveniji med leti 2010-2020 - kateri življenjski dejavniki vplivajo na trajanje bolniškega staleža glede na dano regijo ter leto. 

Za svojo analizo sem potreboval 6 tabel iz Sursa ter eno tabelo iz NIJZ-ja:
1. Bolniški stalež po regijah
2. Izobrazba po regijah
3. Povprečna bruto plača po regijah
4. Število prebivalcev po statističnih regijah
5. Stopnja zelo nizke delovne intenzivnosti
6. Stopnja brezposelnosti po statističnih regijah
7. Delež zmožnosti gospodinjstva, da počitnikuje


## Skupna tabela
Vse zgoraj navedene tabele sem združil v eno samo, ki vsebuje vse podatke, ki so bili nadaljnje uporabljeni v tem projektu.
Atributi (stolpci) so: leto (integer), regija.oznaka (character), delež ljudi z visoko izobrazbo (double) ter delež ljudi z nizko (double), delež ljudi, ki so si zmožni privoščiti počitek (double), stopnja nizke delovne intenzivnosti(integer), povprečna bruto plača (integer), stopnja brezposelnosti (double), prebivalstvo (double) ter bolniški stalež (double).


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
