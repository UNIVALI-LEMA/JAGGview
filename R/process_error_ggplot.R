process_error_ggplot <- function(df, palette, title_y = "Process Error on log(Biomass)") {
  ggplot() +
    geom_ribbon(data = df, fill = my_pal[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = my_pal[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2)) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_line(data = df, aes(x = year, y = mu),
              size = 1) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = 3) +
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    labs(x = "Year", y = title_y) +
    my_theme()
}