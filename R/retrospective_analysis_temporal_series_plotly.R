retrospective_analysis_temporal_series_plotly <- function(lista_dfs, var, title_y) {
  n_cores <- length(unique(lista_dfs$dados$id))

  my_palette <- colorRampPalette(jet_colors)(n_cores)
  
  lista_plots_plotly <- lapply(unique(lista_dfs$dados$Scenario), function(sc){
    dados <- lista_dfs$dados %>%
      filter(Scenario == sc, Index == var)

    max_y <- max(dados$uci)

    dados_rho <- lista_dfs$dados_rho %>%
      filter(Scenario == sc, Index == var)

    plot_ly(colors = my_palette) %>% 
      add_ribbons(
        data = dados %>% filter(id == "Ref"),
        x = ~Year,
        ymin = ~lci,
        ymax = ~uci,
        fillcolor = "rgba(182, 186, 187, 0.64)",
        line = list(width = 0),
        hoverinfo = "text",
        text = ~paste0(
          "CI(95): (", mil_milhao(lci), ") - (", mil_milhao(uci), ")"
        )
      ) %>%
      add_lines(
        data = dados %>% filter(teste == TRUE),
        x = ~Year,
        y = ~mu,
        color = ~as.factor(id),
        type = "scatter",
        mode = "lines",
        line = list(width = 2),
        hoverinfo = "text+x",
        text = ~paste0(
          "mu (", id,"): ", mil_milhao(mu)
        )
      ) %>%
      add_text(
        data = dados_rho,
        x = ~x,
        y = max_y,
        text = ~paste0("ρ= ", mil_milhao(rho, decimals = 3))
      ) %>%
      layout(
        separators = ".",
        title = sc,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          hoverformat = ".0f"
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