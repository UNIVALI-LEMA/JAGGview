retrospective_analysis_surplus_production_ggplot <- function(
  df_lists, var, title_y = "Surplus Production (t)", title_x = "Biomass (t)"
) {
  surplus_data <- df_lists$surplus_data
  rho_data <- df_lists$rho_data
  max <- round_up_to_nearest(max(
    surplus_data$SP[surplus_data$id == "Ref" & surplus_data$Index == var]))
  ggplot() +
    geom_line(data = surplus_data,
        aes(x = SB_i, y = SP, group = id, colour = id), linewidth = 1) +
    geom_text(data = filter(rho_data, Index == var),
        aes(x = Inf, y = Inf,
            label = paste0("rho == ", round(rho, 3))),
        hjust = 1.5, vjust = 2, parse = TRUE) +
    facet_wrap(~Scenario, ncol = 3, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max)) +
    labs(x = title_x, y = title_y, colour = "") +
    my_theme() +
    theme(
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    )
}