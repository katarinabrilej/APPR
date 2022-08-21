# 2. faza: Uvoz podatkov

source("lib/libraries.r")

regije.slo = tibble(
  Regija = c(
    "Gorenjska",
    "Goriška",
    "Jugovzhodna",
    "Koroška",
    "Obalno-kraška",
    "Osrednjeslovenska",
    "Podravska",
    "Pomurska",
    "Posavska",
    "Primorsko-notranjska",
    "Savinjska",
    "Zasavska",
    "SLOVENIJA"
  ),
  `Oznaka regije` = c(
    "kr", "ng", "nm", "sg", "kp", "lj", "mb", "ms", "kk", "po", "ce", "za", "slo"
  )
)
# Vektor let se pri združevanju pogosto ponavlja --> shranim ga v svojo spremenljivko
VEKTOR_LET <- c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')


# Branje tabele o izobrazbi po regijah
izobrazba_po_regijah_raw <- read_excel(
  "podatki/izobrazba-po-regijah.xlsx",
  col_names = TRUE,
) 

izobrazba_po_regijah <- izobrazba_po_regijah_raw %>% 
  left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  mutate(
    `Izobrazba - Nizka` = `Osnovnošolska ali manj - Skupaj` + `Srednješolska - Skupaj`
  )  %>%
  dplyr::rename(
    `Izobrazba - Visoka` = `Višješolska, visokošolska - Skupaj`
  ) %>%
  mutate(
    `delez-visoke-izobrazbe` = `Izobrazba - Visoka` / `Izobrazba - SKUPAJ`,
    `delez-nizke-izobrazbe` = `Izobrazba - Nizka` / `Izobrazba - SKUPAJ`
  ) %>% 
  dplyr::select(
    - `Osnovnošolska ali manj - Skupaj`,
    - `Srednješolska - Skupaj`,
    - `Izobrazba - Visoka`,
    - `Izobrazba - Nizka`,
    - `Izobrazba - SKUPAJ`,
    - `Spol`,
    - `Osnovnošolska`,
    - `Visokošolska 3. stopnje ipd.`,
    - `Visokošolska 2. stopnje ipd.`,
    - `Visokošolska 1. stopnje ipd.`,
    - `Brez izobrazbe, nepopolna osnovnošolska`,
    - `Nižja poklicna, srednja poklicna`,
    - `Srednja strokovna, srednja splošna`,
    - Regija
  )

  
# Branje tabele o deležu prebivalcev vsake regije, ki si lahko privoščijo počitnice
zmoznost_pocitnikovanja_raw <- read_csv2(
  "podatki/zmoznost-gospodinjstva-da-pocitnikuje.csv",
  col_names=TRUE,
  col_types= cols(
    .default = col_guess(),
    '2008' = col_skip(),
    '2009' = col_skip(),
    '2010' = col_skip(),
  )
) 

zmoznost_pocitnikovanja <- zmoznost_pocitnikovanja_raw %>% 
  dplyr::rename(Regija = statisticna_regija) %>%
  left_join(
    regije.slo,
    by="Regija"
  ) %>%
  pivot_longer(
    VEKTOR_LET,
    names_to='leto',
    values_to='indeks-zmoznosti-pocitnikovanja', 
  ) %>% 
  mutate(
    `delez-zmoznosti-pocitnikovanja` = `indeks-zmoznosti-pocitnikovanja` / 100
  ) %>%
  dplyr::select(-c(Regija, `indeks-zmoznosti-pocitnikovanja`))


# Podatki o prebivalstvu o regijah. Popis števila prebivalstva poteka samo na 
# vsake par let (tukaj imam podatke o letih 2011, 2015 ter 2018), zato bom
# ob združevanju podatke za ostale leta ob danih regijah zanemaril
prebivalstvo_po_regijah_raw <- read_csv2(
  "podatki/prebivalstvo-po-regijah.csv",
  skip=1,
  col_types = cols(
    .default = col_guess(),
    SPOL = col_skip(),
    LETO = col_character(),
  )
) 

prebivalstvo_po_regijah <- prebivalstvo_po_regijah_raw %>%
  pivot_longer(
    regije.slo$Regija,
    names_to='Regija',
    values_to='prebivalstvo'
  ) %>% left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  dplyr::select(-c(Regija)) %>%
  dplyr::rename(leto = 'LETO')
  
# Stopnja prebivalstva z nizko delovno intenzivnostjo
stopnja_nizke_delovne_intenzivnosti_raw <- read_csv2(
  "podatki/stopnja-zelo-nizke-delovne-intenzivnosti.csv",
  # skip=2
  col_names=TRUE,
  col_types = cols(
    .default = col_guess(),
    MERITVE = col_skip(),
    LETO = col_character(),
  )
)

stopnja_nizke_delovne_intenzivnosti <- stopnja_nizke_delovne_intenzivnosti_raw %>%
  pivot_longer(
    regije.slo$Regija,
    names_to='Regija',
    values_to='stopja_nizke_delovne_intenzivnosti'
  ) %>% left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  dplyr::select(-c(Regija)) %>%
  dplyr::rename(leto = 'LETO')


# Podatki o povprečni bruto plači
povprecna_bruto_placa_raw <- read_csv2(
  "podatki/povprecna-bruto-placa-po-regijah.csv",
  col_names=TRUE,
  col_types = cols(
    .default = col_guess(),
    'PLAČA' = col_skip(),
    'STAROST' = col_skip(),
    'SPOL' = col_skip(),
  )
)

# Opazim, da je ime prvega stolpca Regija, imena naslednjih pa so oblike:
# P<crke> <leto> --> Iz tega pridobim leto
regex_filter <- "P[:alpha:]+ "
names(povprecna_bruto_placa_raw) <- str_replace(
  names(povprecna_bruto_placa_raw),
  regex_filter,
  ""
)

povprecna_bruto_placa <- povprecna_bruto_placa_raw %>% 
  dplyr::rename(Regija = 'STATISTIČNA REGIJA') %>%
  pivot_longer(
    VEKTOR_LET,
    names_to='leto',
    values_to='povprecna_bruto_placa', 
  )  %>% left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  dplyr::select(-c(Regija))


# Podatki o stopnji brezposelnosti
stopnje_brezposelnosti_raw <- read_csv(
  "podatki/stopnje_brezposelnosti_po_regijah.csv",
  col_names=TRUE,
  col_types = cols(
    .default = col_guess(),
    MERITVE = col_skip(),
    '2008' = col_skip(),
    '2009' = col_skip(),
    '2010' = col_skip(),
  )
)

stopnje_brezposelnosti <- stopnje_brezposelnosti_raw %>%
  dplyr::rename(Regija = 'STATISTIČNA REGIJA') %>%
  pivot_longer(
    VEKTOR_LET,
    names_to='leto',
    values_to='stopnja_brezposelnosti', 
  )  %>% left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  dplyr::select(-c(Regija))


# Podatki o številu dni, ki so jih prebivalci posamične statistične regije povprečno
# preživeli na bolniški.
# 14. vrstico izpustimo, saj za nas ni relevantna
bolniski_stalez_raw <- read_csv2(
  "podatki/bolniski-stalez-po-regijah.csv",
  col_names=TRUE,
  col_types = cols(
    .default = col_guess(),
    Spol = col_skip(),
    Kazalnik = col_skip(),
  ),
  skip_empty_rows = TRUE,
)[-c(14), ]

bolniski_stalez <- bolniski_stalez_raw %>%
  dplyr::rename(Regija = 'Statistična regija') %>%
  pivot_longer(
    VEKTOR_LET,
    names_to='leto',
    values_to='bolniski_stalez_v_dnevih', 
  )  %>% left_join(
    regije.slo,
    by="Regija"
  ) %>% 
  dplyr::select(-c(Regija))


# Združevanje vseh podatkov skupno tabelo.
# Bolniški stalež je opazovana spremenljivka, zato jo dam na konec.
skupna_tabela <- izobrazba_po_regijah %>% left_join(
  zmoznost_pocitnikovanja,
  by = c("leto", "Oznaka regije")
  ) %>% 
  left_join(
    stopnja_nizke_delovne_intenzivnosti,
    by = c("leto", "Oznaka regije")
  ) %>% 
  left_join(
    povprecna_bruto_placa,
    by = c("leto", "Oznaka regije")
  ) %>% 
  left_join(
    stopnje_brezposelnosti,
    by = c("leto", "Oznaka regije")
  ) %>% 
  left_join(
    bolniski_stalez,
    by = c("leto", "Oznaka regije")
  )  %>% left_join(
  prebivalstvo_po_regijah,
  by = c("leto", "Oznaka regije"),
  default = -1
) %>% relocate(
  bolniski_stalez_v_dnevih, .after = last_col()
) %>% mutate(
  across(leto, as.double),
  across(bolniski_stalez_v_dnevih, as.double),
)



uvozi_zemljevid_specifikacija_parametrov <- function() {
  url = "https://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/si/gadm36_SVN_shp.zip"
  ime.zemljevida = "gadm36_SVN_1"
  pot.zemljevida = ""
  mapa = "zemljevidi"
  encoding = "Windows-1250"
  force = FALSE
  slo.regije.sp = uvozi.zemljevid(url, ime.zemljevida, pot.zemljevida, mapa, encoding, force)
  slo.regije.map = slo.regije.sp %>% spTransform(CRS("+proj=longlat +datum=WGS84")) # pretvorimo v ustrezen format
  return (slo.regije.map)
}

izdelaj_zemljevid_slovenije_poligoni <- function() {
  slo.regije.map = uvozi_zemljevid_specifikacija_parametrov()
  slo.regije.poligoni = fortify(slo.regije.map)
  slo.regije.poligoni = slo.regije.poligoni %>%
    left_join(
      rownames_to_column(slo.regije.map@data),
      by = c("id" = "rowname")
    ) %>% dplyr::select(
      regija = NAME_1.x, long, lat, order, hole, piece, id, group
    ) %>%
    mutate(
      regija = replace(regija, regija == "Notranjsko-kraška", "Primorsko-notranjska"),
      regija = replace(regija, regija == "Spodnjeposavska", "Posavska")
    )
  return (slo.regije.poligoni)
}

izdelaj_zemljevid_slovenije_centroidi <- function() {
  slo.regije.map = uvozi_zemljevid_specifikacija_parametrov()
  slo.regije.centroidi = slo.regije.map %>% coordinates %>% as.data.frame
  colnames(slo.regije.centroidi) = c("long", "lat")
  
  slo.regije.centroidi = slo.regije.centroidi %>% rownames_to_column() %>%
    left_join(
      rownames_to_column(slo.regije.map@data),
      by = "rowname"
    ) %>%
    dplyr::select(
      regija = NAME_1, long, lat
    ) %>%
    mutate(
      regija = replace(regija, regija == "Notranjsko-kraška", "Primorsko-notranjska"),
      regija = replace(regija, regija == "Spodnjeposavska", "Posavska")
    )
  return (slo.regije.centroidi)
}

# Če želiš zemljevid shraniti še enkrat, pokliči metodo zapisi_regije_in_centroide
zapisi_regije_in_centroide <- function() {
  # Zapiše poligone in centroide v folder zemljevid.
  # Tukaj predpostavljam, da je pri poganjanju tega trenutni direktorij
  # nastavljen na root projekta.
  slo.regije.poligoni = izdelaj_zemljevid_slovenije_poligoni()
  slo.regije.poligoni = slo.regije.poligoni %>% mutate(
    Regija = transformiraj_regije(regije))
  slo.regije.centroidi = izdelaj_zemljevid_slovenije_centroidi()
  slo.regije.centroidi = slo.regije.centroidi %>% mutate(
    Regija = transformiraj_regije(regije))
  slo.regije.poligoni  %>% write_csv("zemljevidi/regije-poligoni.csv")
  slo.regije.centroidi  %>% write_csv("zemljevidi/regije-centroidi.csv") 
}




