runs_tests_plotly <- function(lista_dfs, combinacoes_fits) {
  lista_plots_plotly <- lapply(seq_len(nrow(combinacoes_fits)), function(i) {

    sc <- combinacoes_fits$Scenario[i]
    idx <- combinacoes_fits$Index[i]

    dados_cpue_res <- lista_dfs$cpue_residuals %>% 
      filter(Scenario == sc, Index == idx)

    dados_SE3 <- lista_dfs$SE3 %>%
      filter(Scenario == sc, Index == idx)

    if(nrow(dados_cpue_res) == 0) {
      return(
          empty_plotly("There is no data for this combination of Scenario and Index")
        )
    }

    plot_ly() %>%
      add_segments(
        data = dados_cpue_res,
        x = ~Year,
        xend = ~Year,
        y = ~Ref,
        yend = ~Res,
        line = list(
          color = "black"
        ),
        hoverinfo = "text",
        text = ~paste0(
          "CI(95): (", mil_milhao(lcl), ") - (", mil_milhao(ucl), ")"
        )
      ) %>%
      add_markers(
        data = dados_cpue_res %>% filter(class == "white"),
        x = ~Year,
        y = ~Res,
        marker = list(
          color = "white",
          line = list(
            color = "black",
            width = 2
          )
        ),
        hoverinfo = "text+x",
        text = ~paste0(
          "Residue: ", mil_milhao(Res)
        )
      ) %>%
      add_markers(
        data = dados_cpue_res %>% filter(class == "red"),
        x = ~Year,
        y = ~Res,
        marker = list(
          color = "red",
          line = list(
            color = "black",
            width = 2
          )
        ),
        hoverinfo = "text+x",
        text = ~paste0(
          "Residue: ", mil_milhao(Res)
        )
      ) %>%
      add_text(
        data = dados_SE3,
        x = ~x,
        y = ~y,
        text = ~paste0("p-value = ", pvalue),
        hoverinfo = "skip"
      ) %>%
      layout(
        separators = ".",
        title = paste(sc, "-", idx),
        shapes = list(
          list(
            type = "rect",
            line = list(width = 0),
            y0 = dados_SE3$lcl,
            y1 = dados_SE3$ucl,
            x0 = dados_SE3$ymin,
            x1 = dados_SE3$ymax,
            fillcolor = dados_SE3$class,
            opacity = 0.2,
            layer = "below"
          )
        ),
        yaxis = list(
          title = "Residuals",
          range = c(-0.8, 0.8),
          showgrid = FALSE
        ),
        xaxis = list(
          showgrid = FALSE
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
}