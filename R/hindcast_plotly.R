hindcast_plotly <- function(lista_dfs, combinacoes) {
  n_cores <- length(unique(lista_dfs$dados$retro))

  my_palette <- colorRampPalette(jet_colors)(n_cores)

  lista_plots_plotly <- lapply(seq_len(nrow(combinacoes)), function(i) {

    sc <- combinacoes$Scenario[i]
    idx <- combinacoes$Index[i]

    dados <- lista_dfs$dados %>%
      filter(Scenario == sc, Index == idx)

    dados_hindcast_1 <- lista_dfs$dados_hindcast_1 %>%
      filter(Scenario == sc, Index == idx)

    dados_hindcast_2 <- lista_dfs$dados_hindcast_2 %>%
      filter(Scenario == sc, Index == idx)

    dados_mase <- lista_dfs$dados_mase %>%
      filter(Scenario == sc, Index == idx)

    if(nrow(dados) == 0) {
      return(
          empty_plotly("There is no data for this combination of Scenario and Index")
        )
    }

    plot_ly(colors = my_palette) %>%
    add_ribbons(
      data = dados %>% filter(retro.peels == 0, year >= 2014),
      x = ~year,
      ymin = ~hat.lci,
      ymax = ~hat.uci,
      fillcolor = alpha("gray", alpha = 0.4),
      line = list(width = 0),
      hoverinfo = "text+x",
      text = ~paste0(
        "hat CI(95): (", mil_milhao(hat.lci), ") - (", mil_milhao(hat.uci), ")"
      )
    ) %>%
      add_ribbons(
        data = dados %>% filter(retro.peels == 0, year %in% 1979:2014),
        x = ~year,
        ymin = ~hat.lci,
        ymax = ~hat.uci,
        fillcolor = alpha("gray", alpha = 0.9),
        line = list(width = 0),
        hoverinfo = "text+x",
        text = ~paste0(
          "hat CI(95): (", mil_milhao(hat.lci), ") - (", mil_milhao(hat.uci), ")"
        )
      ) %>%
      add_lines(
        data = dados %>% filter(hindcast == FALSE),
        x = ~year,
        y = ~hat,
        color = ~as.factor(retro),
        type = "scatter",
        mode = "lines",
        line = list(width = 2),
        hoverinfo = "text",
        text = ~paste0(
          "hat (", retro,"): ", mil_milhao(hat)
        ),
        inherit = FALSE
      ) %>%
      add_lines(
        data = dados_hindcast_2,
        x = ~year,
        y = ~hat,
        line = list(width = 2, color = "black"),
        split = ~retro.peels,
        hoverinfo = "text",
        text = ~paste0(
          "hat - hindcast(", retro,"): ", mil_milhao(hat)
        )
      ) %>%
      add_markers(
        data = dados %>% filter(retro.peels == 0, year < 2015),
        x = ~year,
        y = ~obs,
        marker = list(
          size = 10,
          color = "white",
          line = list(
            width = 1,
            color = "black"
          )
        ),
        hoverinfo = "text",
        text = ~paste0(
          "obs: ", mil_milhao(obs)
        )
      ) %>%
      add_markers(
        data = dados_hindcast_1,
        x = ~year,
        y = ~obs,
        color = ~as.factor(retro),
        split = ~retro,
        marker = list(
          size = 10,
          line = list(
            width = 1,
            color = "black"
          )
        ),
        hoverinfo = "text",
        text = ~paste0(
          "hindcast obs (", retro,"): ", mil_milhao(obs)
        )
      ) %>%
      add_markers(
        data = dados_hindcast_1,
        x = ~year,
        y = ~hat,
        color = ~as.factor(retro),
        split = ~retro,
        marker = list(
          size = 5,
          line = list(
            width = 1,
            color = "black"
          )
        ),
        hoverinfo = "text",
        text = ~paste0(
          "hindcast hat (", retro,"): ", mil_milhao(hat)
        )
      ) %>%
      add_text(
        data = dados_mase,
        x = ~x,
        y = ~y,
        hoverinfo = "none",
        text = ~paste0("MASE = ", mil_milhao(MASE, decimals = 3))
      ) %>%
      layout(
        separators = ".",
        title = paste(sc, "-", idx),
        yaxis = list(
          title = "Index",
          showgrid = FALSE
        ),
        xaxis = list(
          title = "Year",
          showgrid = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 1
      )
  })
}
