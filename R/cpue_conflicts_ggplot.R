cpue_conflicts_ggplot <- function(df_lists, palette, title_y = "Residuals") {
  ggplot() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(data = df_lists$cpue_residuals,
                 aes(x = Year, xend = Year, y = Ref, yend = Res,
                     colour = Index)) +
    geom_point(data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res, fill = Index, colour = Index),
               pch = 21, size = 2) +
    geom_smooth(data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res), se = TRUE, colour = "black") +
    geom_text(data = df_lists$dados_RMSE,
              aes(x = x, y = y, label = paste0("RMSE = ", Value, " %"))) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = 3) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
    scale_fill_manual(values = my_pal) +
    scale_colour_manual(values = my_pal) +
    labs(x = "Year", y = title_y, fill = "", colour = "") +
    my_theme() +
    theme(legend.position = "top")
}