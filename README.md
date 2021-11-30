# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza izseljevanja in priseljevanja v Evropskih državah.

Analiziral bom gibanje prebivalstva v Evropskih državah v odvisnosti od različnih socialnih, ekonomskih in izobraževalnih vplivov. Analiziral bom posebaj izseljevanje in priseljevanje, ter probal ugotoviti vzroke obeh. Napovedal bom selitvene trende v Evropskih državah v bližnji prihodnosti. Večino podatkov sem dobil na [Eurostatu](https://ec.europa.eu/eurostat/web/main/data/database?p_p_id=NavTreeportletprod_WAR_NavTreeportletprod_INSTANCE_nPqeVbPXRmWQ&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view), razen število prebivalcev na km^2, ki sem dobil na [Wikipediji](https://en.wikipedia.org/wiki/Area_and_population_of_European_countries)
 Za ta namen bom uporabil tri tabele.
 
*1. tabela:* **Migracijski tokovi**
    * države (character)
    * BDP države na prebivalca (double)
    * število prebivalcev na km^2 (double)
    * leto (integer)
    * spol (character)
    * izseljevanje (double)
    * priseljevanje (double)

*2. tabela:* **Izseljevanje**
    * država (character)
    * spol (character)
    * leto (integer)
    * stopnja izobrazbe (character)
    * število poučevanih tujih jezikov na učenca (double)
    * zločin (character)
    * število zločinov (double)
    * zaposljivost mladih (double)

*3. tabela:* **Priseljevanje**
    * "država (character)"
    * "spol (character)"
    * "leto (integer)"
    * "stopnja izobrazbe (character)"
    * "število poučevanih tujih jezikov na učenca (double)"
    * "zločin (character)"
    * "število zločinov (integer)"
    * "zaposljivost mladih (double)"
    
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
