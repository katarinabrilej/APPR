# 4. faza: Napredna analiza podatkov

library(sp)
library(rgdal) # funkcija readOGR
library(raster) # funkcija crop
library(rgeos) # funkcija gBuffer
library(tidyverse)
library(cluster)
library(ggalt)

# Uvoz podatkov

brezposelnost_po_regijah = read_csv("uvoz/brezposelnost_po_regijah.csv", col_names=TRUE)
bdp_po_regijah = read_csv("uvoz/bdp_po_regijah.csv", col_names=TRUE)
kazalniki_bolniskega_staleza_moski = read_csv("uvoz/kazalniki_bolniskega_staleza_moski.csv", col_names=TRUE)
kazalniki_bolniskega_staleza_zenske = read_csv("uvoz/kazalniki_bolniskega_staleza_zenske.csv", col_names=TRUE)
kazalniki_bolniskega_staleza = read_csv("uvoz/kazalniki_bolniskega_staleza.csv", col_names=TRUE)

slo.regije.centroidi = read_csv("zemljevidi/regije-centroidi.csv", col_names=TRUE)
slo.regije.poligoni = read_csv("zemljevidi/regije-poligoni.csv", col_names=TRUE)


# Helper funkcije

dobi_seznam_regij <- function() {
  # kazalniki_bolniskega_staleza predstavlja eno od sledečih tabel:
  # - kazalniki_bolniskega_staleza
  # - kazalniki_bolniskega_staleza_moski
  # - kazalniki_bolniskega_staleza_zenske
  return ((kazalniki_bolniskega_staleza_moski %>% filter(Regija != 'slo'))$Regija)
}

pripravi_podatke_za_kmeans_analizo <- function(df) {
  # Da bom lahko naredil prostorsko analizo po tem, stran vržem podatke o Sloveniji
  kazalniki_bolniskega_staleza_brez_slovenije = kazalniki_bolniskega_staleza_moski%>% 
    filter(Regija != 'slo')
  # Shranim seznam regij
  seznam_regij = kazalniki_bolniskega_staleza_brez_slovenije$Regija
  # Odvržem kategorične podatke --> metoda voditeljev deluje zgolj z numeričnimi podatki
  kazalniki_bolniskega_staleza_za_analizo = kazalniki_bolniskega_staleza_brez_slovenije %>% 
    dplyr::select(-Regija, -'Oznaka regije', -Spol) 
  kazalniki_bolniskega_staleza_skalirani = kazalniki_bolniskega_staleza_za_analizo %>% scale()
  rownames(kazalniki_bolniskega_staleza_skalirani) = seznam_regij
  return (kazalniki_bolniskega_staleza_skalirani)
}

kmeans_metoda <- function(kazalniki_bolniskega_staleza_normalizirani, stevilo_skupin, nstart) {
  # kazalniki_bolniskega_staleza_normalizirani predstavlja normalizirane podatke
  # iz tabele o kazalnikih bolniskega staleza.
  return (
    kmeans(kazalniki_bolniskega_staleza_normalizirani, stevilo_skupin, nstart=nstart)
    )
}

pridobi_podatke_za_clustering_zemljevid <- function(kmeans_rezultat) {
  df = data.frame(Regija = dobi_seznam_regij(), Skupina = factor(kmeans_rezultat$cluster))
  podatki_za_clustering_zemljevid = df %>% 
    left_join(slo.regije.centroidi, by = "Regija") %>% 
    left_join(slo.regije.poligoni, by = "Regija")
  podatki_za_clustering_zemljevid = podatki_za_clustering_zemljevid %>%
    mutate(
      Skupina = as.numeric(as.character(podatki_za_clustering_zemljevid$Skupina))
    )
  return(podatki_za_clustering_zemljevid)
}

izrisi_figuro_clusteringa <- function(podatki_za_clustering_zemljevid) {
  zemljevid = podatki_za_clustering_zemljevid %>% 
  ggplot() +
  geom_polygon(
    mapping = aes(long.y, lat.y, group = group, fill = Skupina),
    color = "grey"
  ) +
  coord_map() +
  scale_fill_binned() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )  
  return (zemljevid)
}

clustering_po_bolniskem_stalezu <- function() {
  # Vrne zemljevid Slovenije, kjer so zgručene posamične regije glede na trajanje
  # bolniškega staleža od leta 2008 do leta 2012
  kazalniki_bolniskega_staleza_normalizirani = pripravi_podatke_za_kmeans_analizo(kazalniki_bolniskega_staleza_zenske)
  kmeans_rezultat = kmeans_metoda(kazalniki_bolniskega_staleza_normalizirani, 5, 1000)
  podatki_za_clustering_zemljevid = pridobi_podatke_za_clustering_zemljevid(kmeans_rezultat)
  zemljevid = izrisi_figuro_clusteringa(podatki_za_clustering_zemljevid)
}









