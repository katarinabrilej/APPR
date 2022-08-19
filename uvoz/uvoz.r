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
                          locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO)
izseljevanje <- read_csv("podatki/izseljevanje.csv", na=":", col_select = c(1,2,6,7),
                        locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO)
smrtnost_novorojenckov <- read_csv("podatki/smrtnost_novorojenčkov.csv", na=":", col_select = c(1,2,5),
                                  locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, RATE = Value)
st_ucenih_tj_jezikov <- read_csv("podatki/st_ucenih_tujih_jezikov.csv", na=":", col_select =c(1,2,4,5),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, EDUCATION = ISCED11)
st_obsojenih_zlocinov <- read_csv("podatki/stevilo_umorov_in_spolnih_zlorab.csv", na=":", col_select = c(1,2,3,7),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, CRIME = ICCS)
zaposljivost_mladih <- read_csv("podatki/zaposljivost_mladih.csv", na=":", col_select = c(1,2,3,6,7),
                                locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, EDUCATION = ISCED11, RATE=Value)
bdp_na_prebivalca = read_csv("podatki/bdp_na_prebivalca.csv", na =":", col_select = c(1,2,5),
                             locale=locale(encoding="Windows-1250")) %>% dplyr::rename(YEAR = TIME, COUNTRY = GEO, VALUE = Value)
link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_area"
stran <- html_session(link) %>% read_html()
velikost_drzav <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",") %>% select(c(2,3)) %>% dplyr::rename(COUNTRY = State)

smrtnost_novorojenckov$YEAR = as.integer(smrtnost_novorojenckov$YEAR)
st_obsojenih_zlocinov$YEAR = as.integer(st_obsojenih_zlocinov$YEAR)
st_ucenih_tj_jezikov$YEAR = as.integer(st_ucenih_tj_jezikov$YEAR)
zaposljivost_mladih$YEAR = as.integer(zaposljivost_mladih$YEAR)

pomozna_tabela = tibble(COUNTRY = c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Switzerland",
  "Cyprus",
  "Czechia",
  "Germany (until 1990 former territory of the FRG)",
  "Denmark",
  "Spain",
  "Estonia",
  "Finland",
  "France",
  "United Kingdom",
  "Greece",
  "Hungary",
  "Croatia",
  "Ireland",
  "Iceland",
  "Italy",
  "Lichtenstein",
  "Lithuania",
  "Luxembourg",
  "Latvia",
  "Netherlands",
  "Norway",
  "North Macedonia",
  "Malta",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Sweden"
),
  okrajsava = c(
    "AUT",
    "BEL",
    "BGR",
    "CHE",
    "CYP",
    "CZE",
    "DEU",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "GBR",
    "GRC",
    "HUN",
    "HRK",
    "IRL",
    "ISL",
    "ITA",
    "LIE",
    "LTU",
    "LUX",
    "LVA",
    "NLD",
    "NOR",
    "MKD",
    "MLT",
    "POL",
    "PRT",
    "ROU",
    "SVK",
    "SVN",
    "SWE"
  )
)

