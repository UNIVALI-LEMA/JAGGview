cpue_conflicts_plotly <- function(lista_dfs, dados_RMSE, paleta) {
  lista_plots_plotly <- lapply(unique(lista_dfs$Scenario), function(nome){
    dados <- lista_dfs %>%
      filter(Scenario == nome)

    RMSE <- dados_RMSE %>%
      filter(Scenario == nome)

    plot_ly() %>%
      add_segments(
        data = dados,
        x = ~Year,
        xend = ~Year,
        y = ~Ref,
        yend = ~Res,
        color = ~Index,
        colors = paleta,
        hoverinfo = "none"
      ) %>%
      add_markers(
        data = dados,
        x = ~Year,
        y = ~Res,
        color = ~Index,
        colors = paleta,
        marker = list(
          line = list(width = 0)
        ),
        hoverinfo = "text+x",
        text = ~paste0(
          "Index: ", Index,"<br>Residuals: ", mil_milhao(Res)
        )
      ) %>%
      add_lines(
        data = dados,
        x = ~Year,
        y = ~fit,
        line = list(width = 2, color = "black"),
        hoverinfo = "text",
        text = ~paste0(
          "Loess: ", mil_milhao(fit)
        )
      ) %>%
      add_ribbons(
        data = dados,
        x = ~Year,
        ymin = ~lower,
        ymax = ~upper,
        fillcolor = "gray",
        opacity = 0.3,
        line = list(width = 0),
        hoverinfo = "text",
        text = ~paste0(
          "CI(95): (", mil_milhao(lower), ") - (", mil_milhao(upper), ")"
        )
      ) %>%
      add_text(
        data = RMSE,
        x = ~x,
        y = ~y,
        text = ~paste0("RMSE = ", Value, "%"),
        hoverinfo = "skip"
      ) %>%
      layout(
        separators = ".",
        title = nome,
        xaxis = list(
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Residuals",
          showgrid = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
}