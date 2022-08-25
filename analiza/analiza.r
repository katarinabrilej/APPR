# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

################################### LINEARNA REGRESIJA #####################################################
# Z linearno regresijo napovedujem, kakšen bo bolniški stalež v prihodnjih letih na Gorenjskem

lin_regresija_podatki <- skupna_tabela %>%
  filter(`Oznaka regije` == "kr")

g <- lin_regresija_podatki %>%
  ggplot(
    aes(x=leto, y=bolniski_stalez_v_dnevih, color=leto)
  ) +
  scale_x_continuous(breaks= seq(2011, 2025, by=1)) + 
  geom_point() +
  xlim(2011, 2025) +
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

graf8 <-g +
  geom_line(
    data=data.frame(leto=seq(2010, 2025, 1), napovedi = napovedi),
    aes(
      x=leto,
      y=napovedi
    ),
    color="black"
  )

kv <- lm(data=lin_regresija_podatki, bolniski_stalez_v_dnevih ~ I(leto^2))
g + geom_smooth(method="lm", formula = y ~ x + I(x^2), color="grey")

z <- lowess(lin_regresija_podatki$leto, lin_regresija_podatki$bolniski_stalez_v_dnevih)
g + geom_line(data=as.data.frame(z), aes(x=x, y=y), color="grey")


mls <- loess(data=lin_regresija_podatki, bolniski_stalez_v_dnevih ~ leto)
g + geom_smooth(method="loess", color = "grey")

#Ugotovim kateri model ima najmanjšo napako in tega potem izberem.
which.min(sapply(list(lin, kv, mls), function(x) mean((x$residuals^2))))
g + geom_smooth(method="loess", formula = y ~ x, color = "black")



################################# METODA VODITELJEV ####################################################

# Ugotavljam slovenske statistične regije so si podobne v letu 2020

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(123)
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    dplyr::rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}

k_means_podatki <- skupna_tabela %>% filter(
  leto == 2020, `Oznaka regije` != "slo"
)  %>% 
  dplyr::select(
    - prebivalstvo,
  )
seznam_regij <- k_means_podatki$`Oznaka regije`
k_means_podatki <- k_means_podatki %>%
  dplyr::select(
  - "Oznaka regije"
  )

r.km <- k_means_podatki %>% obrisi(hc = FALSE)
k_means_podatki <- k_means_podatki %>% scale()
optimalno.stevilo.skupin <- obrisi.k(r.km)
set.seed(123)

skupine <- kmeans(k_means_podatki[, -1], 
                  optimalno.stevilo.skupin, 
                  nstart=1000
                  )

skupine$tot.withinss
seznam_regij

skupine.zemljevid <- data.frame(
  regija = seznam_regij,
  Skupina = factor(skupine$cluster)
  ) %>%
  as_tibble() %>%
  dplyr::rename(
    `Oznaka regije` = regija
  )
  

graf9 <- skupine.zemljevid %>%
  left_join(slo.regije.centroidi, by = "Oznaka regije") %>% 
  left_join(slo.regije.poligoni, by = "Oznaka regije") %>%
  ggplot() +
  geom_polygon(
    mapping = aes(long.y, lat.y, group = group, fill = Skupina),
    color = "black"
  ) +
  coord_map() +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  ggtitle('Razvrščanje slovenskih regij v skupine z metodo voditeljev') + 
  labs(fill = "Skupine") +
  scale_fill_manual(
    values=c("#999999", "#56B4E9"),
    labels=c(
      'Skupina 1', 
      'Skupina 2'
    )
  )
