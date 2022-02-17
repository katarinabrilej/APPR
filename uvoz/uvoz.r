# 2. faza: Uvoz podatkov


library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(tidyverse)
library(lemon)


sl <- locale("sl", decimal_mark=",", grouping_mark=".")



#1. Notranje selitve med statističnimi regijami

medregijske <- read_csv2("podatki/medregijske.csv",locale=locale(encoding="Windows-1250"), skip = 2,  na=c("", "...", "-"))
colnames(medregijske)[1] <- "regija"
medregijske <- medregijske %>%
  gather(key="leto.regijav", value="stevilo", -regija) %>%
  separate(leto.regijav, c("leto", "regijav"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto),
         regija=gsub("e( |$)", "a\\1", regija) %>% substring(3),
         regijav=gsub("o( |$)", "a\\1", regijav) %>% substring(2))
names(medregijske)[1] <- "regijaiz"
names(medregijske)[2] <- "leto"
names(medregijske)[3] <- "regijapri"
names(medregijske)[4] <- "stevilo"



#2. Meddržavne selitve glede na spol in vrsto selitve (priseljevanje ali izseljevanje)

meddrzavne <- read_csv2("podatki/meddrzavne_spol.csv",locale=locale(encoding="Windows-1250"), skip = 2, na=c("", "...")) %>%
  rename(drzava = "DRŽAVA DRŽAVLJANSTVA") %>%
  gather(key = "leto.spol", value = "stevilo", -drzava, -SELITVE) %>%
  separate(leto.spol, c("leto", "spol"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto))
names(meddrzavne)[1] <- "vrsta"
names(meddrzavne)[2] <- "drzava"
names(meddrzavne)[3] <- "leto"
names(meddrzavne)[4] <- "spol"
names(meddrzavne)[5] <- "stevilo" 


#3. Priseljenci glede na namen in državo predhodnega bivališča
namen_priseljevanja <- read_csv2("podatki/priseljeni_namen.csv",locale=locale(encoding="Windows-1250"), skip = 2, na=c("", "...", "z") )%>%
  rename(drzava= "DRZAVA DRŽAVLJANSTVA") %>%
  gather(key="leto.namen", value="stevilo", -drzava) %>%
  separate(leto.namen, c("leto", "namen"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto))
names(namen_priseljevanja)[1] <- "drzava"
names(namen_priseljevanja)[2] <- "leto"
names(namen_priseljevanja)[3] <- "namen"
names(namen_priseljevanja)[4] <- "stevilo"

#filter(drzava != 'Srbija', drzava != 'Črna Gora')
#namen_priseljevanja <- namen_priseljevanja

#4. Priseljenci glede na izobrazbo in državo predhodnega bivališča

izobrazba_priseljeni <- read_csv2("podatki/priseljeni_izobrazba.csv",locale=locale(encoding="Windows-1250"), skip = 2, na=c("", "...")) %>%
  rename(drzava = "DRŽAVA PREJŠNJEGA PREBIVALIŠČA") %>%
  gather(key = "leto.izobrazba", value = "stevilo", -drzava, -SPOL) %>%
  separate(leto.izobrazba, c("leto", "izobrazba"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto))
names(izobrazba_priseljeni)[1] <- "spol"
names(izobrazba_priseljeni)[2] <- "drzava"
names(izobrazba_priseljeni)[3] <- "leto"
names(izobrazba_priseljeni)[4] <- "izobrazba"
names(izobrazba_priseljeni)[5] <- "stevilo"


#5. Izseljenci glede na izobrazbo in starostno skupino

izobrazba_izseljeni <- read_csv2("podatki/odseljeni_izobrazba.csv",locale=locale(encoding="Windows-1250"), skip = 2, na=c("", "...")) %>%
  rename(starost = "STAROSTNE SKUPINE") %>%
  gather(key = "leto.izobrazba", value = "stevilo", -starost, -SPOL) %>%
  separate(leto.izobrazba, c("leto", "izobrazba"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto))
names(izobrazba_izseljeni)[1] <- "spol"
names(izobrazba_izseljeni)[2] <- "starost"
names(izobrazba_izseljeni)[3] <- "leto"
names(izobrazba_izseljeni)[4] <- "izobrazba"
names(izobrazba_izseljeni)[5] <- "stevilo"



#6.Izseljenci glede na status aktivnosti in državo prihodnjega bivališča

izseljeni_status_akt <- read_csv2("podatki/odseljeni_status_akt.csv",locale=locale(encoding="Windows-1250"), skip = 2, na=c("", "...")) %>%
  rename(drzava = "DRŽAVA PRIHODNJEGA PREBIVALIŠČA") %>%
  gather(key = "leto.status", value = "stevilo", -drzava, -SPOL) %>%
  separate(leto.status, c("leto", "status"), "(?<=[0-9]) ") %>%
  mutate(leto=parse_number(leto))
names(izseljeni_status_akt)[1] <- "spol"
names(izseljeni_status_akt)[2] <- "drzava"
names(izseljeni_status_akt)[3] <- "leto"
names(izseljeni_status_akt)[4] <- "status"
names(izseljeni_status_akt)[5] <- "stevilo"