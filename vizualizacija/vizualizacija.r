# 3. faza: Vizualizacija podatkov

source("lib/libraries.r")


graf1 <- skupna_tabela %>% ggplot(
  mapping = aes(
    x = leto, 
    y = bolniski_stalez_v_dnevih, 
    color = `Oznaka regije`,
  )
) +
  geom_line() +
  scale_x_continuous(breaks= seq(2011, 2020, by=1)) + 
  xlab('Leto') + 
  ylab("Bolniški stalež (v dnevih)") +
  ggtitle("Spreminjanje bolniškega staleža po slovenskih regijah skozi leta") + 
  theme_light()


graf21 <- skupna_tabela %>% 
  group_by(`Oznaka regije`) %>% 
  summarise(povprecni_bolniski_stalez=mean(bolniski_stalez_v_dnevih)) %>% 
  ggplot(
    aes(`Oznaka regije`, fill = povprecni_bolniski_stalez)
  ) +
  geom_bar(
    aes(y=povprecni_bolniski_stalez),
    stat='identity',
  ) + 
  theme_classic() +
  xlab('') +
  ggtitle("Povprečni bolniški stalež po regijah v letih 2011-2020") +
  ylab("Povprečni bolniški stalež") +
  guides(fill=guide_legend(title="Bolniški stalež"))

graf22 <- skupna_tabela %>%
  group_by(`Oznaka regije`) %>%
  summarise(povprecna_bruto_placa_cez_leta=mean(povprecna_bruto_placa)) %>% 
  ggplot(
    aes(`Oznaka regije`, fill = povprecna_bruto_placa_cez_leta)
  ) +
  geom_bar(
    aes(y=povprecna_bruto_placa_cez_leta),
    stat='identity'
  ) + 
  theme_classic() +
  xlab('') +
  ggtitle("Povprečna bruto plača po regijah v letih 2011-2020") +
  ylab("Povprečna bruto plača") +
  guides(fill=guide_legend(title="Bruto plača"))

g_graf21 <- ggplotGrob(graf21)
g_graf22 <- ggplotGrob(graf22)
maxWidth = grid::unit.pmax(g_graf21$widths[2:5], g_graf22$widths[2:5])
g_graf21$widths[2:5] <- as.list(maxWidth)
g_graf22$widths[2:5] <- as.list(maxWidth)


graf3 <- skupna_tabela %>%
  ggplot(
    mapping = aes(
      group = `Oznaka regije`,
      x = `Oznaka regije`,
      y = bolniski_stalez_v_dnevih,
    )
  ) + 
  geom_boxplot() +
  xlab("Regija") + 
  ylab("Bolniški stalež") + 
  theme_minimal()



# facet_wrap
# povprecna bruto placa && bolniski stalez; leto

# x osi - regije

# skupna_tabela %>% group_by(regija.oznaka) %>% summarise(povprecje=mean(bolniski_stalez))
# npr. bar plot
  
# tocka-plot

# Tak primer kot ga ima Pavla na grafu 1







slo.regije.centroidi = read_csv("zemljevidi/regije-centroidi.csv", col_names=TRUE)
slo.regije.poligoni = read_csv("zemljevidi/regije-poligoni.csv", col_names=TRUE)

# Helper funkcije za graf 1

dobi_imena_novih_letnic <- function(df, dodana_pripona) {
  imena_stolpcev = colnames(df)
  zacetno_leto = "2008"
  ime_stolpca_zacetnega_leta = zlepi_dve_imeni_z_podcrtajem(dodana_pripona, zacetno_leto)
  zacetni_indeks = match(ime_stolpca_zacetnega_leta, imena_stolpcev)
  return (imena_stolpcev[-1: -(zacetni_indeks-1)])
}

dobi_imena_novih_letnic_bolniski_stalez <- function(df) {
  return (dobi_imena_novih_letnic(df, "stalez"))
}

dobi_imena_novih_letnic_bdp <- function(df) {
  return (dobi_imena_novih_letnic(df, "bdp"))
}

dobi_imena_novih_letnic_brezposelnost <- function(df) {
  return (dobi_imena_novih_letnic(df, "brezposelnost"))
}

zlepi_dve_imeni_z_podcrtajem <- function(ime1, ime2) {
  return (paste(ime1, ime2, sep="_"))
}

ustvari_komponento_grafa1 <- function(mapping) {

  graf = kazalniki_bolniskega_staleza %>%
    ggplot(
      mapping = mapping
    ) + geom_line(
      color = 'grey70',
      size = 1.5
    ) + 
    geom_point(
      colour = 'grey20',
      size = 3,
    ) +
    theme_classic() + ylim(8, 27)
  
  return (graf)
}

# Graf 1

ustvari_graf1 <- function() {
  # prikazana_leta = c("2008", "2012", "2016", "2020")
  
  mapping2008 = aes(x = Regija, y = stalez_2008)
  mapping2012 = aes(x = Regija, y = stalez_2012)
  mapping2016 = aes(x = Regija, y = stalez_2016)
  mapping2020 = aes(x = Regija, y = stalez_2020)
  
  return (grid.arrange(
    arrangeGrob(ustvari_komponento_grafa1(mapping2008), ustvari_komponento_grafa1(mapping2012), ncol=2),
    arrangeGrob(ustvari_komponento_grafa1(mapping2016), ustvari_komponento_grafa1(mapping2020), ncol=2),
    nrow = 2
  ))
}


# Helper funkcije za graf 2 in 3 - Primerjava bolniškega staleža skozi leta ter spol

dobi_podatke_za_prikazovanje_staleza_skozi_leta <- function(df) {
  # Pridobi podatke, s katero vizualiziram podatke za 3. graf
  # Spremenljivka df predstavlja eno izmed sledečih tabel:
  # - kazalniki_bolniskega_staleza_moski
  # - kazalniki_bolniskega_staleza_zenske
  # tukaj bom predstavil leta 2008, 2012, 2016, 2020 - dve
  # od teh let bom predstavil tudi v poročilu
  podatki_za_prikazovanje_staleza_skozi_leta = df %>% 
    dplyr::select(Regija, stalez_2010, stalez_2018) %>% 
    filter(Regija != 'slo') %>% 
    left_join(slo.regije.centroidi, by = "Regija") %>% 
    left_join(slo.regije.poligoni, by = "Regija")
  return (podatki_za_prikazovanje_staleza_skozi_leta)
}

komponenta_zemljevida_modra_leto_2010 <- function(df) {
  komponenta_grafa3_2010 = dobi_podatke_za_prikazovanje_staleza_skozi_leta(df) %>% 
    ggplot() +
    geom_polygon(
      mapping = aes(long.y, lat.y, group = group, fill = stalez_2010),
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
  return (komponenta_grafa3_2010)
}

komponenta_zemljevida_barvasta_leto_2018 <- function(df) {
  komponenta_grafa3 = dobi_podatke_za_prikazovanje_staleza_skozi_leta(df) %>% 
    ggplot() +
    geom_polygon(
      mapping = aes(long.y, lat.y, group = group, fill = stalez_2018),
      color = "grey"
    )  +
    coord_map() +
    scale_fill_binned(type = "viridis") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )  
  return (komponenta_grafa3)
}

komponenta_zemljevida_barvasta_leto_2010 <- function(df) {
  komponenta_grafa3_2010 = dobi_podatke_za_prikazovanje_staleza_skozi_leta(df) %>% 
    ggplot() +
    geom_polygon(
      mapping = aes(long.y, lat.y, group = group, fill = stalez_2010),
      color = "grey"
    ) +
    coord_map() +
    scale_fill_binned(type = "viridis") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )  
  return (komponenta_grafa3_2010)
}

komponenta_zemljevida <- function(komponenta_grafa3_delno_izdelana) {
  komponenta_grafa3 = komponenta_grafa3_delno_izdelana +
    coord_map() +
    scale_fill_binned(type = "gradient") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )  
  return (komponenta_grafa3)
}

# Graf 2

ustvari_graf2 <- function() {
  return (grid.arrange(arrangeGrob(
    komponenta_zemljevida_modra_leto_2010(kazalniki_bolniskega_staleza_moski), 
    komponenta_zemljevida_barvasta_leto_2018(kazalniki_bolniskega_staleza_moski), 
    ncol = 2
    ), 
    nrow=1)
    )
}
  
# Graf 3

ustvari_graf3 <- function() {
  return (grid.arrange(arrangeGrob(
    komponenta_zemljevida_barvasta_leto_2010(kazalniki_bolniskega_staleza_zenske), 
    komponenta_zemljevida_modra_leto_2010(kazalniki_bolniskega_staleza_moski), 
    ncol = 2
  ), 
  nrow=1)
  )
}

# Definicija grafov 1, 2 in 3

# g1 = ustvari_graf1()
# g2 = ustvari_graf2()
# g3 = ustvari_graf3()
