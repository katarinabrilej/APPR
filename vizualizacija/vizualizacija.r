# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(viridis)
##############
graf1 = migracije %>% filter(SEX == "Total") %>% ggplot() + aes(x=YEAR, y=EMIGRATION, color = COUNTRY) + geom_line() +
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

graf7 = priseljevanje_2 %>% filter(SEX == "Total", EDUCATION_LANG == "Upper secondary education") %>% ggplot() + aes(x = YEAR, y=EMIGRATION, color = Value.x) + geom_point() +
  xlab("Year") + ylab("Emigration") + facet_wrap(. ~ COUNTRY) + theme(axis.text.x = element_text(angle=90)) + labs(color = "Povprečno število učenih tujih jezikov na drugi stopnji izobrazbe")+
  ggtitle("Število ljudi, ki se je preselilo v državo s povprečnim številom učenih tujih jezikov na drugi stopnji izobrazbe") + scale_color_viridis(option = "G")

print(graf7)
