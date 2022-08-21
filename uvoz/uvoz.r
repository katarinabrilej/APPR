# 2. faza: Uvoz podatkov

library(knitr)
library(readr)
library(tibble)
library(rvest)
library(XML)
library(methods)
library(readxl)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

priseljevanje <- read_csv("podatki/priseljevanje.csv", na=":", col_select = c(1,2,6,7),
                          locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, EMIGRATION = Value)
izseljevanje <- read_csv("podatki/izseljevanje.csv", na=":", col_select = c(1,2,6,7),
                        locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, IMMIGRATION = Value)
smrtnost_novorojenckov <- read_csv("podatki/smrtnost_novorojenčkov.csv", na=":", col_select = c(1,2,5),
                                  locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, RATE = Value)
st_ucenih_tj_jezikov <- read_csv("podatki/st_ucenih_tujih_jezikov.csv", na=":", col_select =c(1,2,4,5),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, EDUCATION_LANG = ISCED11)
st_obsojenih_zlocinov <- read_csv("podatki/stevilo_umorov_in_spolnih_zlorab.csv", na=":", col_select = c(1,2,3,7),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, CRIME = ICCS)
zaposljivost_mladih <- read_csv("podatki/zaposljivost_mladih.csv", na=":", col_select = c(1,2,3,6,7),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, EDUCATION_JOB = ISCED11, RATE=Value)
bdp_na_prebivalca = read_csv("podatki/bdp_na_prebivalca.csv", na =":", col_select = c(1,2,5),
                             locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, GDP = Value)
link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_area"
stran <- html_session(link) %>% read_html()
velikost_drzav <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",") %>% select(c(2,3)) %>% dplyr::rename(COUNTRY = State)

smrtnost_novorojenckov$YEAR = as.integer(smrtnost_novorojenckov$YEAR)
st_obsojenih_zlocinov$YEAR = as.integer(st_obsojenih_zlocinov$YEAR)
st_ucenih_tj_jezikov$YEAR = as.integer(st_ucenih_tj_jezikov$YEAR)
zaposljivost_mladih$YEAR = as.integer(zaposljivost_mladih$YEAR)

priseljevanje = priseljevanje[priseljevanje$COUNTRY != "Montenegro",]

velikost_drzav = velikost_drzav[!(velikost_drzav$COUNTRY %in% c("Russia*", "Ukraine", "Belarus", "Kazakhstan*", "Serbia", "Bosnia and Herzegovina", "Moldova", "Albania", "Turkey*", "Montenegro", "Kosovo", "Azerbaijan*", "Georgia*", "Andorra", "San Marino", "Monaco", "Vatican City", "Armenia", "Total")),]

velikost_drzav$COUNTRY[1] <- "France"
velikost_drzav$COUNTRY[2] <- "Spain"
velikost_drzav$COUNTRY[4] <- "Norway"
velikost_drzav$COUNTRY[8] <- "Italy"
velikost_drzav$COUNTRY[9] <- "United Kingdom"
velikost_drzav$COUNTRY[11] <- "Greece"
velikost_drzav$COUNTRY[15] <- "Portugal"
velikost_drzav$COUNTRY[24] <- "Denmark"
velikost_drzav$COUNTRY[26] <- "Netherlands"

UK = filter(st_obsojenih_zlocinov, COUNTRY %in% c("England and Wales", "Scotland", "Northern Ireland (UK)"))
UK = UK %>% group_by(YEAR, CRIME) %>% summarise(Value = sum(Value))
UK$COUNTRY =  rep("United Kingdom",36)

st_obsojenih_zlocinov = rbind(st_obsojenih_zlocinov,UK)
st_obsojenih_zlocinov = st_obsojenih_zlocinov[!(st_obsojenih_zlocinov$COUNTRY %in% c("England and Wales", "Scotland", "Northern Ireland (UK)")),]

bdp_na_prebivalca$COUNTRY[bdp_na_prebivalca$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
smrtnost_novorojenckov$COUNTRY[smrtnost_novorojenckov$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
st_obsojenih_zlocinov$COUNTRY[st_obsojenih_zlocinov$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
st_ucenih_tj_jezikov$COUNTRY[st_ucenih_tj_jezikov$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
priseljevanje$COUNTRY[priseljevanje$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
izseljevanje$COUNTRY[izseljevanje$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"
zaposljivost_mladih$COUNTRY[zaposljivost_mladih$COUNTRY =="Germany (until 1990 former territory of the FRG)"] = "Germany"

migracije = left_join(priseljevanje, izseljevanje, by = c("COUNTRY", "YEAR", "SEX"))
migracije = left_join(migracije, bdp_na_prebivalca, by = c("COUNTRY", "YEAR"))
migracije = left_join(migracije, velikost_drzav, by = "COUNTRY")

izseljevanje_2 = left_join(izseljevanje, st_ucenih_tj_jezikov, by =  c("COUNTRY", "YEAR"))
izseljevanje_2 = left_join(izseljevanje_2, st_obsojenih_zlocinov, by = c("COUNTRY", "YEAR"))
izseljevanje_2 = left_join(izseljevanje_2, zaposljivost_mladih, by = c("COUNTRY", "YEAR", "SEX"))

priseljevanje_2 = left_join(priseljevanje, st_ucenih_tj_jezikov, by =  c("COUNTRY", "YEAR"))
priseljevanje_2 = left_join(priseljevanje_2, st_obsojenih_zlocinov, by = c("COUNTRY", "YEAR"))
priseljevanje_2 = left_join(priseljevanje_2, zaposljivost_mladih, by = c("COUNTRY", "YEAR", "SEX"))

