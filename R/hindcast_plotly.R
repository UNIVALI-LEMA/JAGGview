hindcast_plotly <- function(lista_dfs, combinacoes) {
  n_cores <- length(unique(lista_dfs$data$retro))

  my_palette <- colorRampPalette(jet_colors)(n_cores)

  lista_plots_plotly <- lapply(seq_len(nrow(combinacoes)), function(i) {

    sc <- combinacoes$Scenario[i]
    idx <- combinacoes$Index[i]

    data <- lista_dfs$data %>%
      filter(Scenario == sc, Index == idx)

    hindcast_data_1 <- lista_dfs$hindcast_data_1 %>%
      filter(Scenario == sc, Index == idx)

    hindcast_data_2 <- lista_dfs$hindcast_data_2 %>%
      filter(Scenario == sc, Index == idx)

    mase_data <- lista_dfs$mase_data %>%
      filter(Scenario == sc, Index == idx)

    if(nrow(data) == 0) {
      return(
          empty_plotly("There is no data for this combination of Scenario and Index")
        )
    }

    plot_ly(colors = my_palette) %>%
    add_ribbons(
      data = data %>% filter(retro.peels == 0, year >= 2014),
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
        data = data %>% filter(retro.peels == 0, year %in% 1979:2014),
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
        data = data %>% filter(hindcast == FALSE),
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
        data = hindcast_data_2,
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
        data = data %>% filter(retro.peels == 0, year < 2015),
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
        data = hindcast_data_1,
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
        data = hindcast_data_1,
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
        data = mase_data,
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
