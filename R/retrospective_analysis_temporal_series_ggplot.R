retrospective_analysis_temporal_series_ggplot <- function(df_lists, var, title_y) {
  data <- df_lists$data
  rho_data <- df_lists$rho_data
  max <- round_up_to_nearest(max(
    data$uci[data$id == "Ref" & data$Index == var]))
  min <- round_up_to_nearest(max(
    data$lci[data$id == "Ref" & data$Index == var]))
  
  min <- ifelse(min > 0, 0, min)
  ggplot() +
    geom_ribbon(data = filter(data, id == "Ref", Index == var),
        aes(x = Year, ymin = lci, ymax = uci),
        fill = "gray80") +
    geom_line(data = filter(data, teste == TRUE, Index == var),
        aes(x = Year, y = mu, colour = id), linewidth = 1) +
    geom_text(data = filter(rho_data, Index == var),
        aes(x = Inf, y = Inf,
            label = paste0("rho == ", round(rho, 3))),
        hjust = 1.5, vjust = 2, parse = TRUE) +
    facet_wrap(~ Scenario, ncol = 3, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), limits = c(min, max)) +
    labs(x = "Year", y = title_y, colour = "") +
    my_theme() +
    theme(
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    )
}