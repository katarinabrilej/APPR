# 2. faza: Uvoz podatkov

library("readxl")
library("dplyr")

# Definiranje encodinga
loc = locale(
  date_names = "sl", 
  date_format = "%AD", 
  time_format = "%AT", 
  decimal_mark = ".", 
  grouping_mark = ",", 
  tz = "UTC", 
  encoding = "windows-1250", 
  asciify = FALSE
  )

# Imena regij bomo preimenovali na sledeč način 
# (da ne bomo imeli šumnikov ter celotnih besed)
oznake_regij = c(
  "Gorenjska"="kr",
  "Goriška"="ng",
  "Jugovzhodna"="nm", 
  "Koroška"="sg", 
  "Obalno kraška"="kp",
  "Osrednjeslovenska"="lj", 
  "Podravska"="mb", 
  "Pomurska"="ms", 
  "Posavska"="kk",
  "Primorsko-notranjska"="po", 
  "Savinjska"="ce", 
  "Zasavska"="za", 
  "SLOVENIJA"="slo"
)


# Branje podatkov o brezposelnosti po regijah - izpustim stolpec "MERITVE"
brezposelnost_po_regijah = read_csv(
  "stopnje_brezposelnosti_po_regijah.csv", 
  col_names=TRUE, 
  locale = loc,
  col_types = cols(
    .default = col_guess(),
    MERITVE = col_skip()
  )
)

# Branje podatkov o brezposelnosti po regijah
bdp_po_regijah = read_csv(
  "bdp_po_regijah.csv",
  col_names = TRUE,
  locale = loc,
  col_types = cols(
    .default = col_guess(),
    MERITVE = col_skip()
  )
)


# Branje podatkov o trajanju bolniškega staleža - izpustim stolpec "Tip podatka"
kazalniki_bolniskega_staleza = read_excel(
  "kazalniki_bolniškega_staleža_po_statističnih_regijah_in_spolu.xlsx",
  col_names = TRUE,
  col_types = c(
    "guess", "guess", "guess", "skip", 
    "guess", "guess", "guess", "guess", "guess", "guess", "guess", 
    "guess", "guess", "guess", "guess", "guess", "guess")
  )


dopolni_stolpec_spol <- function(spol) {
  # Ker je tabela razdeljena glede na spol, vrednost atributa spol izgleda takole:
  # spol = ["Moški", NA, NA, ..., NA, "Ženska", NA, NA, ..., NA]
  # Tukaj to popravim na ["m", "m", ..., "m", "f", "f", ..., "f"]
  # ter to vstavim nazaj v podatkovno tabelo
  spol_trenutni = first(spol)
  for (index in 1: 14) {
    spol[index] = spol_trenutni
  }
  spol = sub("Moški", "m", spol)
  spol_trenutni = nth(spol, 15)
  for (index in 15: length(spol)) {
    spol[index] = spol_trenutni
  }
  spol = sub("Ženske", "f", spol)
  return(spol)
}

odstrani_vrstico_neznano <- function(df) {
  # Dve vrstici zasedata vrednost pri regiji "neznano" ter za tem podatke tipa "NA".
  # oznaka regije je zapisana pod vrednostjo 99, zato ti dve vrstici tukaj odstranim.
  return (df[(df$`Oznaka regije` != 99), ])
}

transformiraj_regije <- function(regije) {
  for (i in 1:length(regije)) {
    regije[i] = as.character(oznake_regij[regije[i]])
  }
  return (regije)
}

preimenuj_letnice_v_tabeli <- function(df, dodana_pripona) {
  imena_stolpev = colnames(df)
  zacetni_indeks_preimenovanja = match("2008", imena_stolpev)
  for (k in zacetni_indeks_preimenovanja:length(imena_stolpev)) {
    imena_stolpev[k] = paste(imena_stolpev[k], dodana_pripona, sep="_")
  }
  colnames(df) = imena_stolpev
  return (df)
}

transformiraj_kazalnike_bolniskega_staleza <- function(
  kazalniki_bolniskega_staleza) {
  # kazalniki_bolniskega_staleza je specificna tabela, ki jo preberem iz podatkov
  # To je zapakirano v funkciji zgolj zaradi preglednosti
  # Ta metoda uporabi funkcionalnost zgornjih "helper" metod ter naredi sledece:
  #   - Dopolne stolpec "spol"
  #   - Odstrani odvečni dve vrstici pod vrednostjo "regija = neznano"
  #   - Transformira stolpec "regija" (tretji stolpec v opazovani tabeli)
  #   - Preimenuje stolpce, ki držijo vrednosti o časovnih komponentah
  
  spol = first(kazalniki_bolniskega_staleza)
  kazalniki_bolniskega_staleza = kazalniki_bolniskega_staleza %>% mutate(
    Spol = dopolni_stolpec_spol(spol))
  kazalniki_bolniskega_staleza = odstrani_vrstico_neznano(kazalniki_bolniskega_staleza)
  regije = nth(kazalniki_bolniskega_staleza, 3)
  kazalniki_bolniskega_staleza = kazalniki_bolniskega_staleza %>% mutate(
    Regija = transformiraj_regije(regije))
  kazalniki_bolniskega_staleza = preimenuj_letnice_v_tabeli(
    kazalniki_bolniskega_staleza, "stalez")
  return (kazalniki_bolniskega_staleza)
}

transformiraj_brezposelnost_in_bdp_tabeli <- function(df, pripona) {
  # To je funkcija, ki hkrati transformira tabeli brezposelnost_po_regijah
  # bdp_po_regijah. 
  # Ta metoda uporabi funkcionalnost zgornjih "helper" metod ter naredi sledece:
  #   - preimenuje `STATISTICNA REGIJA` v 'Regija'
  #   - Transformira stolpec "Regija" (2. stolpec v dani tabeli)
  #   - Preimenuje stolpce, ki držijo vrednosti o časovnih komponentah
  #     (tukaj uposteva atribut "pripona" v funkciji)
  imena_stolpcev = colnames(df)
  # `STATISTICNA REGIJA` je prvi stolpec v obeh tabelah
  imena_stolpcev[1] = "Regija"
  colnames(df) = imena_stolpcev
  regije = first(df)
  df = df %>% mutate(
  Regija = transformiraj_regije(regije))
  df = preimenuj_letnice_v_tabeli(
    df, pripona)
  return (df)
}

kazalniki_bolniskega_staleza = transformiraj_kazalnike_bolniskega_staleza(
  kazalniki_bolniskega_staleza
)

brezposelnost_po_regijah = transformiraj_brezposelnost_in_bdp_tabeli(
  brezposelnost_po_regijah, "brezposelnost"
)

bdp_po_regijah = transformiraj_brezposelnost_in_bdp_tabeli(
  bdp_po_regijah, "brezposelnost"
)






