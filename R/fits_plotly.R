fits_plotly <- function(lista_dfs, combinacoes_fits, paleta) {
  lista_plots_plotly <- lapply(seq_len(nrow(combinacoes_fits)), function(i) {

    sc <- combinacoes_fits$Scenario[i]
    idx <- combinacoes_fits$Index[i]

    if(nrow(lista_dfs$CI_95 %>% filter(Scenario == sc, Index == idx)) == 0) {
      return(
          empty_plotly("There is no data for this combination of Scenario and Index")
        )
    }

    plot_ly() %>%
      add_ribbons(
        data = lista_dfs$CI_95 %>% filter(Scenario == sc, Index == idx),
        x = ~Year,
        ymin = ~lci,
        ymax = ~uci,
        inherit = FALSE,
        fillcolor = paleta[1],
        opacity = 0.3,
        line = list(width = 0),
        hoverinfo = "text+x",
        text = ~paste0(
          "CI(95): (", mil_milhao(lci), ") - (", mil_milhao(uci), ")"
        )
      ) %>%
      add_ribbons(
        data = lista_dfs$CI_80 %>% filter(Scenario == sc, Index == idx),
        x = ~Year,
        ymin = ~lci,
        ymax = ~uci,
        inherit = FALSE,
        fillcolor = paleta[1],
        opacity = 0.3,
        line = list(width = 0),
        hoverinfo = "text",
        text = ~paste0(
          "CI(80): (", mil_milhao(lci), ") - (", mil_milhao(uci), ")"
        )
      ) %>%
      add_lines(
        data = lista_dfs$CI_80 %>% filter(Scenario == sc, Index == idx),
        x = ~Year,
        y = ~mu,
        type = "scatter",
        mode = "lines",
        line = list(width = 2, color = "black"),
        hoverinfo = "text",
        text = ~paste0("Value: ", mil_milhao(mu))
      ) %>%
      add_markers(
        data = lista_dfs$Li_Ui %>% filter(Scenario == sc, Index == idx, !is.na(Mean)),
        x = ~Year,
        y = ~Mean,
        marker = list(
          color = "white",
          line = list(
            color = "black",
            width = 2
          )
        ),
        error_y = list(
          type = "data",
          array = ~error,
          color = "black"
        ),
        hoverinfo = "text",
        text = ~paste0(
          "Point: ", mil_milhao(Mean), 
          "<br>Interval: (", mil_milhao(Li), ") - (", mil_milhao(Ui), ")"
        )
      ) %>%
      layout(
        separators = ".",
        title = paste(sc, "-", idx),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
  names(lista_plots_plotly) <- paste(
    combinacoes_fits$Scenario, combinacoes_fits$Index, sep = "_"
  )
  return(lista_plots_plotly)
}