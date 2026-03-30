process_error_plotly <- function(df, paleta, titulo_y) {
  lista_plots_plotly <- lapply(unique(df$Scenario), function(nome){
    dados <- df %>%
      filter(Scenario == nome)

    plot_ly() %>%
      add_ribbons(
        data = dados,
        x = ~year,
        ymin = ~lcl2,
        ymax = ~ucl2,
        fillcolor = paleta[1],
        opacity = 0.3,
        inherit = FALSE,
        line = list(width = 0),
        hoverinfo = "text+x",
        text = ~paste0(
          "CI(90): (", mil_milhao(lcl2), ") - (", mil_milhao(ucl2), ")"
        )
      ) %>%
      add_ribbons(
        data = dados,
        x = ~year,
        ymin = ~lcl,
        ymax = ~ucl,
        fillcolor = paleta[1],
        opacity = 0.3,
        inherit = FALSE,
        line = list(width = 0),
        hoverinfo = "text",
        text = ~paste0(
          "CI(97,5): (", mil_milhao(lcl), ") - (", mil_milhao(ucl), ")"
        )
      ) %>%
      add_lines(
        data = dados,
        x = ~year,
        y = ~mu,
        type = "scatter",
        mode = "lines",
        line = list(width = 2, color = "black"),
        hoverinfo = "text",
        text = ~paste0("Value: ", mil_milhao(mu))
      ) %>%
      layout(
        separators = ".",
        title = nome,
        xaxis = list(
          showgrid = FALSE
        ),
        yaxis = list(
          title = titulo_y,
          showgrid = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
  return(lista_plots_plotly)
}