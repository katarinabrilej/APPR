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
podatki.ucni = podatki.ucni[-c(1,7,8,10,12,21,28,30),]

g <- ggplot(podatki.ucni, aes(x=RATE, y=EMIGRATION)) + geom_point()
g + geom_smooth(method="lm")

lmodel = lm(data = podatki.ucni, EMIGRATION ~ Value.x + RATE + GDP )
lmodel
napovedi = predict(lmodel)
print(napovedi)

kv = lm(data = podatki.ucni, EMIGRATION ~ I(Value.x^2) + I(Value.y^2) + RATE + I(GDP^2))
mls = loess(data = podatki.ucni, EMIGRATION ~ Value.x + RATE + GDP )

sapply(list(lin.model, kv, mls), function(x) mean((x$residuals^2)))

formula1 = EMIGRATION ~ GDP + RATE +Value.x

napaka.cv = function(podatki, k, formula) {
  n = nrow(podatki)
  r = sample(1:n)
  
  razrez = cut(1:n, k, labels = FALSE)
  
  razbitje = split(r, razrez)
  
  pp.napovedi = rep(0, n)
  for (i in 1:length(razbitje)) {
    #Naučimo se modela na množici S/Si
    model = podatki[ -razbitje[[i]], ] %>% lm(formula = formula)
    #Naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = predict(object = model, newdata = podatki[ razbitje [[i]], ] )
    
  }
  
  #model so učni podatki in ostalo ( razbitje[[i]]) so testni podatki. 
  
  napaka = mean((pp.napovedi - podatki$EMIGRATION)^2)
  return(napaka)
}

set.seed(111)
napaka.cv(podatki.ucni, 5, formula1)


formule = c(
  EMIGRATION ~  GDP + RATE + Value.x,
  EMIGRATION ~  GDP + I(RATE^2) + I(Value.x^2),
  EMIGRATION ~  GDP + RATE,
  EMIGRATION ~  GDP + I(GDP^2) + RATE + I(Value.x^2),
  EMIGRATION ~  GDP + I(GDP^2) + RATE + I(RATE^2)+ Value.x + I(Value.x^2),
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + RATE + I(RATE^2) + I(RATE^3) + Value.x + I(Value.x^2) + I(Value.x^3),
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + I(GDP^4) + RATE + I(RATE^2) + I(RATE^3)+ I(RATE^4) + Value.x + I(Value.x^2) + I(Value.x^3) + I(Value.x^4),
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + I(GDP^4) + I(GDP^5) + RATE + I(RATE^2) + I(RATE^3)+ I(RATE^4) + I(RATE^5) + Value.x + I(Value.x^2) + I(Value.x^3) + I(Value.x^4) + I(Value.x^5),
  EMIGRATION ~  GDP + I(GDP^2) + RATE + I(RATE^2),
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + RATE + I(RATE^2) + I(RATE^3) ,
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + I(GDP^4) + RATE + I(RATE^2) + I(RATE^3)+ I(RATE^4),
  EMIGRATION ~  GDP + I(GDP^2) + I(GDP^3) + I(GDP^4) + I(GDP^5) + RATE + I(RATE^2) + I(RATE^3)+ I(RATE^4) + I(RATE^5)
)

napake = rep(0,12)
for (i in 1:12){
  formula = formule[[i]]
  set.seed(111)
  napaka = napaka.cv(podatki.ucni, 5, formula)
  napake[i] = napaka
}

which.min(napake)

formula2=EMIGRATION ~  GDP + I(RATE^2) + I(Value.x^2) ####ima najmanjšo napako
set.seed(111)
napaka.cv(podatki.ucni, 5, formula2)

lin.model=lm(data=podatki.ucni, formula = formula2)

napaka_regresije = function(podatki, model) {
  set.seed(111)
  podatki %>%
    bind_cols(EMIGRATION.hat = predict(model, podatki)) %>%
    mutate(
      izguba = (EMIGRATION - EMIGRATION.hat) ^ 2
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}
napaka_regresije(podatki.ucni, lin.model)

library(ranger)
library(janitor)
p.ucni <- janitor::clean_names(podatki.ucni)
set.seed(111)

ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(emigration.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (emigration - emigration.hat) ^ 2
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(emigration.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (emigration != emigration.hat)
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

set.seed(111)
lin.model = p.ucni %>% ucenje(emigration ~ gdp + I(gdp^2) + I(gdp^3) +rate + I(rate^2) + I(rate^3) + value_x + I(value_x^2) + I(value_x^3) +  value_y + I(value_y^2) + I(value_y^3) ,"lin.reg")
napaka_regresije(p.ucni, lin.model, "lin.reg")

set.seed(111)
ng.reg.model = p.ucni %>%ucenje(emigration ~ gdp +rate + value_x + value_y ,"ng")
napaka_razvrscanja(p.ucni, ng.reg.model, "ng")

formula3 = emigration ~ gdp + I(gdp^2) + I(gdp^3) +rate + I(rate^2) + I(rate^3) + value_x + I(value_x^2) + I(value_x^3) +  value_y + I(value_y^2) + I(value_y^3)

library(iml)

X = p.ucni %>% dplyr :: select(emigration, gdp,rate, value_x, value_y)

set.seed(111)
pfun = function(model, newdata) {
  predict(model, data = newdata, predict.all = FALSE)$predictions
}  

ng.reg.pred = Predictor$new(
  ng.reg.model,
  data = X, y = p.ucni$emigration,
  predict.fun = pfun
)

ng.reg.moci = FeatureImp$new(ng.reg.pred, loss = "mse")

plot(ng.reg.moci)

############

ger = priseljevanje_bdp %>% 
  filter(COUNTRY == "Germany", SEX == "Total", CRIME == "Intentional homicide", EDUCATION_LANG == "Upper secondary education - general", EDUCATION_JOB == "All ISCED 2011 levels")


CAC = ger[,4]
CACs = ger[,c(1,4)]


Lag <- function(x, n){
  (c(rep(NA, n), x)[1 : length(x)] )
}
naredi.df <- function(x){data.frame(EMIGRATION = x,
                                    EMIGRATION1 = Lag(x, 1),
                                    EMIGRATION2 = Lag(x, 2) ,
                                    EMIGRATION3 = Lag(x, 3),
                                    EMIGRATION4 = Lag(x, 4)
)
}
set.seed(111)
df = naredi.df(CAC$EMIGRATION)
model.bi = ranger(EMIGRATION ~ EMIGRATION1 +EMIGRATION2 + EMIGRATION3 + EMIGRATION4, data=df %>% drop_na())

nap = nrow(df)
for (i in 1:5){
  set.seed(111)
  df <- naredi.df(c(df$EMIGRATION, NA))
  napoved = predict(model.bi,  data = df[nap + i, ] )$predictions
  df[nap+i, 1] = napoved
}

napovedi = df[c(9,9,10,11,13), 1]
CACs2 <- CACs
CACs2[c( 9, 10, 11, 12,13),2] = napovedi
CACs2[c(9, 10, 11, 12,13),1] = c(2020, 2021, 2022, 2023, 2024)

napoved_graf <- ggplot(CACs2) + geom_bar(aes(x = YEAR, y = EMIGRATION, fill = YEAR > 2019), stat = "identity") +
  scale_fill_manual(name = 'Napovedi', values = setNames(c("#53B400",'midnight blue'),c(T, F))) +
  xlab('Leto') + ylab('Priseljevanje') + ggtitle("Napoved priseljevanja v Nemčijo v letih 2020-2024")

print(napoved_graf)

