# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(viridis)
##############
graf1 = migracije %>% filter(SEX == "Total",) %>% ggplot() + aes(x=YEAR, y=EMIGRATION, color = COUNTRY) + geom_line() +
  xlab("Year") + ylab("Emigration") + theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Število ljudi, ki se je v letih preselilo v opazovane države") + labs(color = "Države v katere so se ljudje selili")

graf2 =  migracije %>% filter(SEX == "Total") %>% ggplot() + aes(x=YEAR, y=IMMIGRATION, color = COUNTRY) + geom_line() +
  xlab("Year") + ylab("Immigration") + theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Število ljudi, ki se je v letih preselilo iz opazovanih držav") + labs(color = "Države iz katerih so se ljudje selili")

graf3 = migracije %>% filter(YEAR==2009, SEX == "Total") %>% ggplot(mapping=aes(group=COUNTRY, x = COUNTRY, y=EMIGRATION, color = GDP)) + geom_point() +
  xlab("Country") + ylab("Emigration") + theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Število ljudi, ki se je v letu 2009 preselilo v opazovane države") + labs(color = "BDP države na prebivalca")

graf4 = migracije %>% filter(YEAR==2009, SEX == "Total") %>% ggplot(mapping=aes(group=COUNTRY, x = COUNTRY, y=IMMIGRATION, color = GDP)) + geom_point() +
  xlab("Country") + ylab("Immigration") + theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Število ljudi, ki se je v letu 2009 preselilo iz opazovanih držav") + labs(color = "BDP države na prebivalca")

graf5 = migracije %>% ggplot() + aes(x = YEAR, y = EMIGRATION, color = SEX) + geom_line() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Število ljudi, ki se je v letih preselilo v opazovane države po spolih") + labs(color = "Spol")

graf6 = migracije %>% ggplot() + aes(x = YEAR, y = IMMIGRATION, color = SEX) + geom_line() +
  xlab("Year") + ylab("Imigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Število ljudi, ki se je v letih preselilo iz opazovanih držav po spolih") + labs(color = "Spol")

#########

graf7 = priseljevanje_2 %>% filter(SEX == "Total", EDUCATION_LANG == "Upper secondary education", EDUCATION_JOB == "All ISCED 2011 levels", CRIME == "Rape") %>% ggplot() + aes(x = YEAR, y=EMIGRATION, color = Value.x) + geom_point() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=45)) + labs(color = "Povprečno število učenih tujih jezikov na drugi stopnji izobrazbe")+
  ggtitle("Število ljudi, ki se je preselilo v državo s povprečnim številom učenih tujih jezikov na tretji stopnji izobrazbe") + scale_color_viridis(option = "G")

graf8 = izseljevanje_2 %>% filter(SEX == "Total", EDUCATION_LANG == "Upper secondary education", EDUCATION_JOB == "All ISCED 2011 levels", CRIME == "Rape") %>% ggplot() + aes(x = YEAR, y=IMMIGRATION, color = Value.x) + geom_point() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=45)) + labs(color = "Povprečno število učenih tujih jezikov na drugi stopnji izobrazbe")+
  ggtitle("Število ljudi, ki se je izselilo iz države s povprečnim številom učenih tujih jezikov na tretji stopnji izobrazbe") + scale_color_viridis(option = "G")

graf14 = priseljevanje_2 %>% filter(SEX == "Total", EDUCATION_LANG == "Primary education", EDUCATION_JOB == "All ISCED 2011 levels", CRIME == "Rape") %>% ggplot() + aes(x = YEAR, y=EMIGRATION, color = Value.x) + geom_line() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=45)) + labs(color = "Povprečno število učenih tujih jezikov na drugi stopnji izobrazbe")+
  ggtitle("Število ljudi, ki se je preselilo v državo s povprečnim številom učenih tujih jezikov na prvi stopnji izobrazbe") + scale_color_viridis(option = "G")

graf15 = izseljevanje_2 %>% filter(SEX == "Total", EDUCATION_LANG == "Primary education", EDUCATION_JOB == "All ISCED 2011 levels", CRIME == "Rape") %>% ggplot() + aes(x = YEAR, y=IMMIGRATION, color = Value.x) + geom_line() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=45)) + labs(color = "Povprečno število učenih tujih jezikov na prvi stopnji izobrazbe")+
  ggtitle("Število ljudi, ki se je izselilo iz države s povprečnim številom učenih tujih jezikov na prvi stopnji izobrazbe") + scale_color_viridis(option = "G")

########

graf9 = migracije %>% filter(SEX == "Females") %>% group_by(COUNTRY) %>% summarise(EMIGRATION_AY = sum(EMIGRATION)) %>% ggplot() + aes(x = COUNTRY, y = EMIGRATION_AY) + 
  geom_bar(stat = "identity") + xlab("Country") + ylab("Emigration all years") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Skupno število žensk, ki se je preselilo v posamezne države")

graf10 = migracije %>% filter(SEX == "Females") %>% group_by(COUNTRY) %>% summarise(IMMIGRATION_AY = sum(IMMIGRATION)) %>% ggplot() + aes(x = COUNTRY, y = IMMIGRATION_AY) + 
  geom_bar(stat = "identity") + xlab("Country") + ylab("Immigration all years") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Skupno število žensk, ki se je preselilo iz posameznih držav")

graf11 = migracije %>% filter(SEX == "Total") %>% group_by(COUNTRY, YEAR) %>% summarise(razlika = EMIGRATION - IMMIGRATION) %>% 
  ggplot() + aes(x = YEAR, y = razlika, color = COUNTRY) + geom_line() + xlab("Year") + ylab("Difference") + ggtitle("Razlika med priseljevanjem in izseljevanjem") + 
  facet_wrap(. ~ COUNTRY)

########

stevilo_zlocinov = izseljevanje_2 %>%group_by(COUNTRY, YEAR, SEX) %>% summarise(CRIME.NO = sum(Value.y)) 
izseljevanje_2 = left_join(izseljevanje_2,stevilo_zlocinov, by=c("COUNTRY", "YEAR", "SEX"))
priseljevanje_2 = left_join(priseljevanje_2,stevilo_zlocinov, by=c("COUNTRY", "YEAR", "SEX"))

#######

graf12 = izseljevanje_2 %>% filter(SEX == "Total", YEAR == 2015, EDUCATION_JOB=="All ISCED 2011 levels", EDUCATION_LANG =="Primary education", CRIME =="Rape") %>% ggplot() + aes(x=COUNTRY, y = CRIME.NO, fill = IMMIGRATION) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x = "Države", y = "Število zločinov", title="Število zločinov v državah s številom ljudi, ki so se preselili iz držav v letu 2015", fill = "Izseljevanje") + 
  scale_fill_viridis(option="G")

graf13 = priseljevanje_2 %>% filter(SEX == "Total", YEAR == 2015, EDUCATION_JOB=="All ISCED 2011 levels", EDUCATION_LANG =="Primary education", CRIME =="Rape") %>% ggplot() + aes(x=COUNTRY, y = CRIME.NO, fill = EMIGRATION) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x = "Države", y = "Število zločinov", title="Število zločinov v državah s številom ljudi, ki so se preselili v države v letu 2015", fill ="Priseljevanje") + 
  scale_fill_viridis(option="G")

######
graf16 = priseljevanje_2 %>% filter(SEX == "Males", YEAR == 2013, EDUCATION_JOB == "All ISCED 2011 levels", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = EMIGRATION, color = RATE) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Priseljevanje", color = "Stopnja zaposljivosti mladih", title= "Priseljevanje moških v države v letu 2013 s stopnjo zaposljivosti mladih s končanimi vsemi stopnjami izobrazbe") +
  scale_color_viridis(option="G")

graf17 = izseljevanje_2 %>% filter(SEX == "Males", YEAR == 2013, EDUCATION_JOB == "All ISCED 2011 levels", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = IMMIGRATION, color = RATE) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Izseljevanje", color = "Stopnja zaposljivosti mladih", title= "Izseljevanje moških iz držav v letu 2013 s stopnjo zaposljivosti mladih s končanimi vsemi stopnjami izobrazbe") +
  scale_color_viridis(option="G")

######
graf18 = priseljevanje_2 %>% filter(SEX == "Females", YEAR == 2015, EDUCATION_JOB == "Less than primary, primary and lower secondary education (levels 0-2)", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = EMIGRATION, color = RATE) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Priseljevanje", color = "Stopnja zaposljivosti mladih", title= "Priseljevanje žensk v države v letu 2015 s stopnjo zaposljivosti mladih s stopnjami izobrazbe 0-2") +
  scale_color_viridis(option="G")

graf19 = izseljevanje_2 %>% filter(SEX == "Females", YEAR == 2015, EDUCATION_JOB == "Less than primary, primary and lower secondary education (levels 0-2)", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = IMMIGRATION, color = RATE) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Izseljevanje", color = "Stopnja zaposljivosti mladih", title= "Izseljevanje žensk iz držav v letu 2015 s stopnjo zaposljivosti mladih s stopnjami izobrazbe 0-2") +
  scale_color_viridis(option="G")

###
graf20 = priseljevanje_2 %>% filter(SEX == "Females", YEAR == 2008, EDUCATION_JOB == "Less than primary, primary and lower secondary education (levels 0-2)", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = EMIGRATION, color = Value.y) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Priseljevanje", color = "Stopnja zaposljivosti mladih", title= "Priseljevanje žensk v države v letu 2008 s številom obsojevih posisltev") +
  scale_color_viridis(option="G")

graf21 = izseljevanje_2 %>% filter(SEX == "Females", YEAR == 2008, EDUCATION_JOB == "Less than primary, primary and lower secondary education (levels 0-2)", EDUCATION_LANG=="Primary education", CRIME == "Rape") %>% 
  ggplot() + aes(x = COUNTRY, y = IMMIGRATION, color = Value.y) + geom_point()+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Izseljevanje", color = "Stopnja zaposljivosti mladih", title= "Izseljevanje žensk iz držav v letu 2008 s številom obsojenih posilstev") +
  scale_color_viridis(option="G")

####
graf22 = migracije %>% filter(SEX == "Total") %>% ggplot() + aes(x=COUNTRY, y=EMIGRATION) +geom_boxplot() +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Priseljevanje",title="Število priseljenih v države")

graf22 = migracije %>% filter(SEX == "Total") %>% ggplot() + aes(x=COUNTRY, y=IMMIGRATION) +geom_boxplot() +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) + labs(x="Države", y= "Izseljevanje",title="Število izseljenih iz držav")

library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(tmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),] %>% dplyr::rename(COUNTRY = name)

Europe$COUNTRY[Europe$COUNTRY == "Czech Rep."] = "Czechia"
Europe$COUNTRY[Europe$COUNTRY == "Macedonia"] = "North Macedonia"

colnames(migracije) = c("YEAR", "COUNTRY", "SEX", "EMIGRATION", "IMMIGRATION", "GDP", "AREA")

mankajoce = data_frame(
  YEAR = rep(NA,14*3),
  COUNTRY = c(rep("Albania",3),
              rep("Andorra",3),
              rep("Bosnia and Herz.",3),
              rep("Belarus",3),
              rep("Kosovo",3),
              rep("Monaco",3),
              rep("Moldova",3),
              rep("Malta",3),
              rep("Montenegro",3),
              rep("Russia",3),
              rep("San Marino",3),
              rep("Serbia",3),
              rep("Ukraine",3),
              rep("Vatican",3)),
  EMIGRATION = rep(NA,42),
  IMMIGRATION = rep(NA,42),
  SEX = rep("Total",42),
  GDP = rep(NA,14*3),
  AREA = rep(NA,14*3)
  
)

migracije_mankajoce = rbind(migracije, mankajoce)
evropa_migracije = left_join(Europe, migracije_mankajoce, by= "COUNTRY")
  
zemljevid_priseljevanje = evropa_migracije %>% filter(SEX == "Total") %>% ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = EMIGRATION)+
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  labs(fill = "Priseljevanje") + ggtitle("Zemljevid preseljevanja v opazovane Evropske države") +
  geom_sf_text(aes(label = COUNTRY), color = "gray", size = 2) +
  scale_fill_viridis(option="G")

zemljevid_izseljevanje = evropa_migracije %>% filter(SEX == "Total") %>% ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = IMMIGRATION)+
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  labs(fill = "Izseljevanje") + ggtitle("Zemljevid Izseljevanja v opazovane Evropske države") +
  geom_sf_text(aes(label = COUNTRY), color = "gray", size = 2) +
  scale_fill_viridis(option="G")

