priors_posteriors_plotly <- function(lista_dfs, combinacoes_priors_posteriors, paleta) {
  lista_plots_plotly <- lapply(seq_len(nrow(combinacoes_priors_posteriors)), function(i) {

    sc <- combinacoes_priors_posteriors$Scenario[i]
    var <- combinacoes_priors_posteriors$Var_Interesse[i]

    var1 <- paste0(var, "01")
    var2 <- paste0(var, "02")

    dados_prior <- lista_dfs$prior %>%
      filter(Scenario == sc) %>%
      select(Scenario, all_of(var1), all_of(var2))

    names(dados_prior) <- c("Scenario", "var1", "var2")

    dados_posterior <- lista_dfs$posterior %>%
      filter(Scenario == sc) %>%
      select(Scenario, all_of(var1), all_of(var2))

    names(dados_posterior) <- c("Scenario", "var1", "var2")

    PPMR <- lista_dfs$PPMR %>%
      filter(Scenario == sc) %>%
      pull(var)

    PPVR <- lista_dfs$PPVR %>%
      filter(Scenario == sc) %>%
      pull(var)

    mult <- lista_dfs$mult %>%
      filter(variavel == var) %>%
      pull(limite)

    if(nrow(dados_posterior) == 0) {
      return(
        empty_plotly(
          "There is no data for this combination of Scenario and Variable of Interest"
        )
      )
    }

    plot_ly() %>%
      add_trace(
        data = dados_prior,
        x = ~var1,
        y = ~var2,
        fillcolor = alpha(paleta[1], 0.5),
        fill = "tozeroy",
        type = "scatter",
        mode = "lines",
        line = list(
          width = 0.5, 
          color = "black"
        ),
        hoverinfo = "text",
        # )
        text = ~paste0(
          "Prior<br>", var, ": ", mil_milhao(var1), "<br>Density: ", mil_milhao(var2)
        )
      ) %>%
      add_trace(
        data = dados_posterior,
        x = ~var1,
        y = ~var2,
        fillcolor = alpha(paleta[2], 0.5),
        fill = "tozeroy",
        type = "scatter",
        mode = "lines",
        line = list(
          width = 0.5, 
          color = "black"
        ),
        hoverinfo = "text",
        text = ~paste0(
          "Posteriori<br>", var, ": ", mil_milhao(var1), "<br>Density: ", mil_milhao(var2)
        )
      ) %>%
      add_text(
        data = lista_dfs$PPMR,
        x = ~x*mult,
        y = ~y*max(dados_posterior$var2),
        text = ~paste0("PPMR = ", PPMR),
        hoverinfo = "none"
      ) %>%
      add_text(
        data = lista_dfs$PPVR,
        x = ~x*mult,
        y = ~y*max(dados_posterior$var2),
        text = ~paste0("PPVR = ", PPVR),
        hoverinfo = "none"
      ) %>%
      layout(
        separators = ".",
        title = paste(sc, "-", var),
        yaxis = list(
          title = "Density",
          showgrid = FALSE
        ),
        xaxis = list(
          title = var,
          showgrid = FALSE,
          range = c(min(dados_prior$var1), mult)
        ),
        showlegend = FALSE,
        hovermode = "x unified",
        hoverdistance = 2
      )
  })
  return(lista_plots_plotly)
}