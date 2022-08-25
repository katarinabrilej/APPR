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

povprecni_bolniski_stalez <- skupna_tabela %>% 
  group_by(`Oznaka regije`) %>% 
  summarise(povprecni_bolniski_stalez=mean(bolniski_stalez_v_dnevih))

graf21 <- povprecni_bolniski_stalez %>% 
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
      y = stopja_nizke_delovne_intenzivnosti,
    )
  ) + 
  geom_boxplot() +
  xlab("Regija") + 
  ylab("Stopnja nizke delovne intenzivnosti po regijah") + 
  ggtitle("Stopnja nizke delovne intenzivnosti") +
  theme_minimal()


graf4 <- skupna_tabela %>%
  ggplot(
    aes(
      x=`delez-zmoznosti-pocitnikovanja`, 
      y=bolniski_stalez_v_dnevih,
      colour=leto,
      )
  ) +
  geom_point() +
  facet_wrap(. ~`Oznaka regije`, ncol=3) +
  stat_smooth(method = lm) +
  theme_bw() +
  xlab("Delež gospodinjstev, ki si je sposoben privoščiti počitnice") +
  ylab("Bolniški stalež") +
  ggtitle("Delež gospodinjstev, ki so si sposobni privoščiti počitnice po regijah")


regije_po_stopnji_izobrazbe <- skupna_tabela %>% 
  group_by(`Oznaka regije`) %>% 
  summarise(
    povprecni_delez_visoke_izobrazbe=mean(`delez-visoke-izobrazbe`),
    povprecni_delez_nizke_izobrazbe=mean(`delez-nizke-izobrazbe`),
    ) %>%
  pivot_longer(
    c(2, 3),
    names_to="type",
    values_to="value"
  ) %>% mutate_if(is.numeric, round, 2)


graf5 <- regije_po_stopnji_izobrazbe %>%
  ggplot(
    mapping = aes(x = `Oznaka regije`, y = value, fill = type)
  ) +
  geom_bar(width = 1, stat = 'identity', color = "black", ylim = c(0, 0.4)) +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color="black") +
  scale_y_continuous(breaks= seq(0, 1, by=0.05)) + 
  scale_fill_manual(
    values=c("#D3D3D3", "#63666A"), 
    labels=c(
      'Povprečni delež nizke izobrazbe', 
      'Povprečni delež visoke izobrazbe'
      )
    ) +
  theme_minimal() +
  xlab("Oznaka regije") +
  ylab("Delež visoke oz. nizke izobrazbe po regijah") +
  ggtitle("Primerjava povprečne višine izobrazbe med regijami v letih 2011-2020") +
  guides(fill=guide_legend(title="Legenda"))



graf61 <- regije_po_stopnji_izobrazbe %>%
  dplyr::filter(`Oznaka regije`=="kr") %>%
  ggplot(
    mapping = aes(x = `Oznaka regije`, y = value, fill = type)
  ) +
  geom_bar(width = 1, stat = 'identity', color="black") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color="black") +
  coord_polar("y", start=0) + 
  ggtitle("Stopnja visoke oz. nizke izobrazbe na Gorenjskem") + 
  xlab('') + 
  ylab('') + 
  scale_fill_manual(
    values=c("#999999", "#56B4E9"),
    labels=c(
      'Povprečni delež nizke izobrazbe', 
      'Povprečni delež visoke izobrazbe'
    )
    ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())


graf62 <- regije_po_stopnji_izobrazbe %>%
  dplyr::filter(`Oznaka regije`=="sg") %>%
  ggplot(
    mapping = aes(x = `Oznaka regije`, y = value, fill = type)
  ) +
  geom_bar(width = 1, stat = 'identity', color="black") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color="black") +
  coord_polar("y", start=0) + 
  ggtitle("Stopnja visoke oz. nizke izobrazbe  na Koroškem") + 
  xlab('') + 
  ylab('') + 
  scale_fill_manual(
    values=c("#999999", "#56B4E9"),
    labels=c(
      'Povprečni delež nizke izobrazbe', 
      'Povprečni delež visoke izobrazbe'
    )
    ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())



slo.regije.centroidi = read_csv("zemljevidi/regije-centroidi.csv", col_names=TRUE)
slo.regije.poligoni = read_csv("zemljevidi/regije-poligoni.csv", col_names=TRUE)


graf7 <- povprecni_bolniski_stalez %>%
  filter(`Oznaka regije` != "slo") %>%
  left_join(slo.regije.centroidi, by = "Oznaka regije") %>% 
  left_join(slo.regije.poligoni, by = "Oznaka regije") %>%
  ggplot() +
    geom_polygon(
      mapping = aes(long.y, lat.y, group = group, fill = povprecni_bolniski_stalez),
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
    ) +
  ggtitle('Povprečni bolniški stalež v letih 2011-2020 na zemljevidu') + 
  labs(fill = "Povprečen bolniški stalež")
