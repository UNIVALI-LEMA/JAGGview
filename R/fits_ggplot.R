fits_ggplot <- function(df_lists, palette, title_y = "Abundance index") {
  ggplot() +
    geom_ribbon(data = df_lists$CI_80,
        aes(x = Year, ymin = lci, ymax = uci),
        alpha = 0.3, fill = palette[1]) +
    geom_ribbon(data = df_lists$CI_95,
        aes(x = Year, ymin = lci, ymax = uci),
        alpha = 0.3, fill = palette[1]) +
    geom_line(data = df_lists$CI_80,
        aes(x = Year, y = mu)) +
    geom_errorbar(data = df_lists$Li_Ui,
                  aes(x = Year, ymin = Li, ymax = Ui)) +
    geom_point(data = df_lists$Li_Ui,
        aes(x = Year, y = Mean),
        pch = 21, fill = "white", size = 1.5) +
    facet_grid(Scenario ~ Index, scales = "free") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4, 0.5)) +
    labs(x = "Year", y = title_y) +
    my_theme()
}