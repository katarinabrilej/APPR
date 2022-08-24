# 4. faza: Napredna analiza podatkov

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(cluster)
library(ggalt)

#### dendogram
set.seed(111)
migracije %>% filter(SEX == "Total", YEAR == 2013) %>% ggplot() +
  geom_point(
  mapping = aes(x = EMIGRATION, y = GDP),
  size = 2
) + geom_label(
  mapping = aes(x = EMIGRATION, y = GDP + 2000, label = COUNTRY)
) +
  theme_classic() + xlab("Priseljevanje") + ylab("BDP na prebivalca") +
  ggtitle("Preseljevanje in BDP na prebivalca v letu 2013")

primeri = migracije %>% filter(SEX == "Total", YEAR == 2013)
primeri = primeri[,c(2,4,6)]
dendogram1 = primeri[,-1] %>% dist() %>% hclust(method = "ward.D")

razdalje = dist(primeri[,-1])
drzave = unlist(primeri[,1])

narisan_dendogram = plot(
  dendogram1,
  label = primeri$COUNTRY,
  ylab = "višina",
  main = NULL)

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r1 = hc.kolena(dendogram1)

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.kolena(r1)

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = EMIGRATION, y = GDP, color = skupina
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

### kolena so 2,3,4,5,7,8,10,11,13,15,16,18,21,23,25,29

##k = 2
skupine.2 = dendogram1 %>% cutree(k = 2) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.2, 2)

##k = 3
skupine.3 = dendogram1 %>% cutree(k = 3) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.3, 3)

##k = 4
skupine.4 = dendogram1 %>% cutree(k = 4) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.4, 4)

### k = 5
skupine.5 = dendogram1 %>% cutree(k = 5) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.5, 5)

### k=7
skupine.7 = dendogram1 %>% cutree(k = 7) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.7, 7)

### k=8
skupine.8 = dendogram1 %>% cutree(k = 8) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.8, 8)

###k=10
skupine.10 = dendogram1 %>% cutree(k = 10) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.10, 10)

###k=11
skupine.11 = dendogram1 %>% cutree(k = 11) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.11, 11)

###k=13
skupine.13 = dendogram1 %>% cutree(k = 13) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.13, 13)

###k=15
skupine.15 = dendogram1 %>% cutree(k = 15) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.15, 15)

###k=16
skupine.16 = dendogram1 %>% cutree(k = 16) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.16, 16)

###k=18
skupine.18 = dendogram1 %>% cutree(k = 18) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.18, 18)

###k=21
skupine.21 = dendogram1 %>% cutree(k = 21) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.21, 21)

### k=23
skupine.23 = dendogram1 %>% cutree(k = 23) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.23, 23)

### k=25
skupine.25 = dendogram1 %>% cutree(k = 25) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.25, 25)

### k=29
skupine.29 = dendogram1 %>% cutree(k = 29) %>% as.ordered()
diagram.skupine(primeri,primeri$COUNTRY, skupine.29, 29)

######obrisi
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
      set.seed(42) # zato, da so rezultati ponovljivi
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

r.hc = primeri[, -1] %>% obrisi(hc = TRUE)
r.km = primeri[, -1] %>% obrisi(hc = FALSE)

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

diagram.obrisi(r.hc)
diagram.obrisi(r.km)

### najboljse k =2 hc in 2 km

drzave.x.y =
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

k = obrisi.k(r.hc)
skupine = primeri[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

k = obrisi.k(r.km)
set.seed(111) # ne pozabimo na ponovljivost rezultatov
skupine = primeri[, -1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()

diagram.skupine2 = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
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

diagram.skupine2(drzave.x.y, drzave.x.y$drzava, skupine, k)

######################
priseljevanje_bdp = left_join(priseljevanje_2, migracije)
priseljevanje_bdp = priseljevanje_bdp[,-c(11,13)]


podatki.ucni = priseljevanje_bdp %>% 
  filter(YEAR == 2019, SEX == "Total", EDUCATION_LANG == "Upper secondary education - general", CRIME == "Intentional homicide", EDUCATION_JOB == "All ISCED 2011 levels")

g <- ggplot(podatki.ucni, aes(x=RATE, y=EMIGRATION)) + geom_point()
g + geom_smooth(method="lm")

lin.model = lm(data = podatki.ucni, EMIGRATION ~ Value.x + Value.y + RATE + GDP )
lin.model
napovedi = predict(lin.model)
print(napovedi)

kv = lm(data = podatki.ucni, EMIGRATION ~ I(Value.x^2) + I(Value.y^2) + RATE + I(GDP^2))
mls = loess(data = podatki.ucni, EMIGRATION ~ Value.x + Value.y + RATE + GDP )

sapply(list(lin.model, kv, mls), function(x) mean((x$residuals^2)))



