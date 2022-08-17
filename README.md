# Analiza vpliva makroekonomskih kazalcev na trajanje bolniškega staleža v Sloveniji


## Opis projekta

Skozi to seminarsko nalogo bom raziskoval ter analiziral povezavo med splošnim makroekonomskim stanjem v Sloveniji ter trajanjem bolniškega staleža glede na regije za leta 2008-2020. 

Za začetek bom kot edini makroekonomski kazalec vzel realno rast BDP-ja ter stopnjo brezposelnosti v Republiki Sloveniji, saj po mojem skupaj najbolje odražata makroekonomsko sliko po eni strani ter položaj delavcev na drugi.

Za primerjavo sem vzel povprečni bolniški stalež po statističnih regijah in spolu od leta 2008 do 2020.

## Podatki

1. Kazalniki bolniškega staleža po statističnih regijah in spolu
    1. Leto
    2. Spol (moški/ ženska)
    3. Regija
    4. Izgubljeni koledarski dnevi na zaposlenega

2. Stopnja brezposelnosti v Sloveniji
    1. Leto
    2. Regija
    3. Stopnja brezposelnosti

3. Realna rast GDP
    1. Leto
    2. Realna rast GDP


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
