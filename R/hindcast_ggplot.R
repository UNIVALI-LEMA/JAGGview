hindcast_ggplot <- function(df_lists) {
  ggplot() +
    geom_ribbon(data = filter(df_lists$dados, retro.peels == 0),
        aes(x = year,
            ymin = hat.lci, ymax = hat.uci),
        fill = "gray80") +
    geom_ribbon(data = filter(df_lists$dados, retro.peels == 0,
                              year %in% 1979:2014),
                aes(x = year,
                    ymin = hat.lci, ymax = hat.uci),
                fill = "gray30", alpha = 0.5) +
    geom_line(data = filter(df_lists$dados, hindcast == FALSE),
              aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = df_lists$dados_hindcast_2,
              aes(x = year, y = hat, group = retro.peels),
              linewidth = 1, colour = "white") +
    geom_point(data = filter(df_lists$dados, retro.peels == 0, year < 2015),
               aes(x = year, y = obs), pch = 21, size = 4,
               fill = "white") +
    geom_point(data = df_lists$dados_hindcast_1, show.legend = FALSE,
               aes(x = year, y = obs, fill = retro),
               pch = 21, size = 4) +
    geom_point(data = df_lists$dados_hindcast_1, show.legend = FALSE,
               aes(x = year, y = hat, fill = retro),
               pch = 21, size = 2) +
    geom_text(data = df_lists$dados_mase,
              aes(x = x, y = y,
                  label = paste0("MASE = ", round(MASE, 3)))) +
    labs(x = "Year", y = "Index", colour = "") +
    facet_wrap(Scenario ~ Index, ncol = 3, drop = FALSE) +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    my_theme() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
}