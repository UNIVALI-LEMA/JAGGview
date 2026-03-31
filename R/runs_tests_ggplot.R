runs_tests_ggplot <- function(df_lists, title_y = "Residuals") {
  ggplot() +
    geom_rect(data = df_lists$SE3,
              aes(xmin = ymin, xmax = ymax, ymin = lcl, ymax = ucl,
                  fill = class),
              alpha = 0.2) +
    geom_text(data = df_lists$SE3,
        aes(x = x, y = y, label = paste0("p-value = ", round(pvalue, 3)))) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(data = df_lists$cpue_residuals,
                 aes(x = Year, xend = Year, y = Ref, yend = Res)) +
    geom_point(data = filter(df_lists$cpue_residuals, class == "white"),
        aes(x = Year, y = Res), fill = "white",
        pch = 21, size = 2) +
    geom_point(data = filter(df_lists$cpue_residuals, class == "red"),
        aes(x = Year, y = Res), fill = "red",
        pch = 21, size = 2.5) +
    facet_grid(Scenario ~ Index, scales = "free") +
    scale_fill_manual(values = c("green", "red")) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.8, 0.8)) +
    labs(x = "Year", y = title_y) +
    my_theme() +
    theme(legend.position = "none")
}