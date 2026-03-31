retrospective_analysis_correlation_plotly <- function(lista_dfs, var, title_y, title_x) {
  n_cores <- length(unique(lista_dfs$surplus_data$id))

  my_palette <- colorRampPalette(jet_colors)(n_cores)
  
  lista_plots_plotly <- lapply(unique(lista_dfs$surplus_data$Scenario), function(sc){
    surplus_data <- lista_dfs$surplus_data %>%
      filter(Scenario == sc)

    max_y <- max(surplus_data$SP)

    max_x <- max(surplus_data$SB_i) * 0.8

    rho_data <- lista_dfs$rho_data %>%
      filter(Scenario == sc, Index == var)

    plot_ly(colors = my_palette) %>% 
      add_lines(
        data = surplus_data,
        x = ~SB_i,
        y = ~SP,
        color = ~as.factor(id),
        type = "scatter",
        mode = "lines",
        line = list(width = 2),
        hoverinfo = "text",
        text = ~paste0(
          "Biomass (", id,"): ", mil_milhao(SB_i), "t<br>Surplus Production (", id,"): ", mil_milhao(SP), "t" 
        )
      ) %>%
      add_text(
        data = rho_data,
        x = max_x,
        y = max_y,
        text = ~paste0("ρ= ", mil_milhao(rho, decimals = 3)),
        hoverinfo = "none"
      ) %>%
      layout(
        separators = ".",
        title = sc,
        xaxis = list(
          title = title_x,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = title_y,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
  return(lista_plots_plotly)
}