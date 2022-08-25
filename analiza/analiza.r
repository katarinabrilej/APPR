# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

# Z linearno regresijo napovedujem, kakšen bo bolniški stalež v prihodnjih letih na Gorenjskem

lin_regresija_podatki <- skupna_tabela %>%
  filter(`Oznaka regije` == "kr")

g <- lin_regresija_podatki %>%
  ggplot(
    aes(x=leto, y=bolniski_stalez_v_dnevih, color=leto, size=2)
  ) +
  scale_x_continuous(breaks= seq(2011, 2020, by=1)) + 
  geom_point() +
  geom_line() + # Razmisli, ce je to potrebno
  theme(legend.position = "none") +
  labs(title = "Napovedovanje bolniškega staleža za Gorenjsko po letih") + 
  xlab("Leto") + 
  ylab("Bolniški stalež")

lin <- lm(
  data=lin_regresija_podatki,
  bolniski_stalez_v_dnevih ~ leto,
)

napovedi <- predict(lin, data.frame(leto=seq(2010, 2025, 1)))

g +
  geom_line(
    data=data.frame(leto=seq(2010, 2025, 1), napovedi = napovedi),
    aes(
      x=leto,
      y=napovedi
    ),
    color="grey"
  )

kv <- lm(data=lin_regresija_podatki, bolniski_stalez_v_dnevih ~ I(leto^2))
g + geom_smooth(method="lm", formula = y ~ x + I(x^2), color="grey")

z <- lowess(lin_regresija_podatki$leto, lin_regresija_podatki$bolniski_stalez_v_dnevih)
g + geom_line(data=as.data.frame(z), aes(x=x, y=y), color="grey")


mls <- loess(data=lin_regresija_podatki, bolniski_stalez_v_dnevih ~ leto)
g + geom_smooth(method="loess", color = "grey")

#Ugotovim kateri model ima najmanjšo napako in tega potem izberem.
which.min(sapply(list(lin, kv, mls), function(x) mean((x$residuals^2))))
graf8 <- g + geom_smooth(method="loess", formula = y ~ x, color = "#63666A")

