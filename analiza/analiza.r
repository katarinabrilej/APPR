# 4. faza: Napredna analiza podatkov
graf7 <- ggplot(test5, aes(x=leto, y=skupaj, group=1)) +
  geom_point(aes(col=vec, size=1)) +
  geom_smooth(method="loess", formula= y~x, color="deeppink", size=0.3) +
  facet_grid(izobrazba ~ .) +
  scale_x_continuous("Leto",  breaks = seq(2010,2020,2)) +
  theme(panel.background = element_rect((fill="white")), axis.text= element_text(size=6), 
        text= element_text(size=10), strip.background =element_rect(fill="white"), 
        strip.text.x = element_text(size = 9)) +
  labs("Analiza izseljenega prebivalstva glede na izobrazbo in starostno") +
  ylab("Število izseljenega prebivalstva glede na izobrazbo") 

graf7 <- graf7 + guides(col=guide_legend(title="Starostne \nskupine prebivalstva")) +
    theme(legend.title = element_text(colour="black", size=10, 
                                      face="bold"))
