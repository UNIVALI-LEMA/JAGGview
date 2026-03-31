trajectories_FFmsy_ggplot <- function(df, palette, title_y = expression(F/F[MSY])) {
  ggplot() +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2)) +
    geom_hline(yintercept = 1, linetype = "longdash") +
    ## geom_hline(yintercept = 0.4, linetype = "longdash", colour = "red") +
    geom_line(data = df, aes(x = year, y = mu),
              size = 1) +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    labs(x = "Year", y = title_y, fill = "",
         colour = "") +
    my_theme() +
    theme(legend.position = "none")
}