# 3. faza: Vizualizacija podatkov


library(tmap)
library(digest)

#Uvoz zemljevida

#zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip","SVN_adm1", encoding = "UTF-8")
#zemljevid$NAME_1 <- c("Gorenjska", "Goriška","Jugovzhodna Slovenija", "Koroška", "Primorsko-notranjska", "Obalno-kraška", 
 #                    "Osrednjeslovenska", "Podravska", "Pomurska", "Savinjska", "Posavska", "Zasavska")


#1.zemljevid: priseljevanje prebivalstva po regijah

#regije_pri <- medregijske %>% group_by(regijapri) %>% summarise(skupaj = sum(stevilo, na.rm = TRUE ))
#regije_pri$regijapri = regije_pri$regijapri %>% trimws()

#podatki_pri = merge(zemljevid, regije_pri, by.x = "NAME_1", by.y = "regijapri" )
#tm_shape(podatki_pri) +
#  tm_polygons("skupaj") + 
#  tm_format("NLD", title="Število prebivalstva, ki se je priselilo iz določene regije", bg.color="white")


#2. zemljevid: izseljevanje prebivalstva po regijah
#regije_izs <- medregijske %>% group_by(regijaiz) %>% summarise(skupaj = sum(stevilo, na.rm = TRUE ))
#regije_izs$regijaiz = regije_izs$regijaiz %>% trimws()

#podatki_izs = merge(zemljevid, regije_izs, by.x = "NAME_1", by.y = "regijaiz" )
#tm_shape(podatki_izs) +
 # tm_polygons("skupaj") +
  #tm_format("NLD", title="Število prebivalstva, ki se je izselilo iz določene regije", bg.color="white")



#Graf 1: Povprečno število prebivalstva, ki se je selilo glede na statistične regije Slovenije

povprecje_regije <- medregijske %>% group_by(regijaiz, regijapri) %>%
  summarise(povprecje=(sum(stevilo)/20))

graf1 <- ggplot(data = povprecje_regije, aes(x=" " ,y=povprecje, fill = regijaiz)) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_wrap(~regijapri, ncol= 6) +
  xlab(" ") + ylab("Povprečno število") +
  ggtitle("Priseljevanje in odseljevanje po statističnih regijah Slovenije") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="black")) +
  theme(axis.text.x = element_text(angle=90)) 
+ coord_flip() + theme_dark() +
  scale_fill_brewer(palette = "BrBG") 


#Graf 2: Število priseljenih in izseljenih ljudi glede na drzavo selitve

povprecje2 <- meddrzavne %>% group_by(drzava, vrsta) %>%
  summarise(povprecje=(sum(stevilo)/20))

graf2 <- ggplot(data=povprecje2, aes(x=drzava, y=povprecje, fill=`vrsta`)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  labs(x = "Država", y = "Povprečno število", title = "Povprečno število v priseljenega ali odseljenega prebivalstva glede na države") +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG")


# Graf 3: Graf, ki prikazuje število priseljenega prebivalstva glede na namen selitve in državo predhodnega bivališča.

graf3 <- ggplot(namen_priseljevanja, aes(x=leto,y=stevilo, group=1)) +
  geom_point(aes(col=drzava, size=stevilo)) +
  facet_grid(namen ~ .) + xlab("Leto") + ylab("Število") +
  ggtitle("Priseljeni prebivalci glede na namen selitve po letih") +
  coord_flip() + theme_dark() +
  scale_fill_brewer(palette = "BrBG")


#Graf 4: Graf, ki prikazuje število priseljenega prebivalstva glede na njihovo izobrazvo in državo predhodnega bivališča

povprecje4 <- izobrazba_priseljeni %>% group_by(drzava, izobrazba) %>%
  summarise(povprecje=(sum(stevilo)/9))

graf4 <- ggplot(izobrazba_priseljeni, aes(x=leto,y=stevilo, group=1)) +
  geom_point(aes(col=drzava, size=stevilo)) +
  facet_grid(izobrazba ~ .) + 
  labs(y="Število ljudi", 
       x="Vrsta izobrazbe", 
       title="Število priseljenega prebivalstva glede na stopnjo izobrazbe") +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG") 

#Graf 5: Graf, ki prikazuje število izseljenega prebivalstva glede na njihovo izobrazbo in starostno skupino

graf5 <- ggplot(data=test5a, aes(x=leto, y=skupaj, col=spol)) +
  geom_line(size=1) + facet_grid(~vec) +
  theme_bw() +
  scale_color_manual(values=c("darkgoldenrod3", "cadetblue")) +
  xlab("Država") + ylab("Povprečno število(/100)") +
  ggtitle("Odseljeni prebivalci glede starostno skupino in spol") +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG") + 
  scale_x_continuous(breaks=seq(2010, 2020, 2))

#Graf 6: Graf, ki prikazuje število izeljenega prebivalstva glede na status aktivosti in državo pihodnjega bivališča

povprecje6 <- izseljeni_status_akt %>% group_by(drzava, status) %>%
  summarise(povprecje=(sum(stevilo)/9)/100)

graf6 <- ggplot(data=povprecje6, aes(x = drzava, y = povprecje, fill = status)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Država") + ylab("Povprečno število(/100)") +
  ggtitle("Odseljeni prebivalci glede na status aktivnosti") +
  coord_flip() +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG") 

