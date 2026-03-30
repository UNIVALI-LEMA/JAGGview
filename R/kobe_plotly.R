kobe_plotly <- function(df) {
  lista_plots_plotly <- lapply(unique(df$tmp11$Scenario), function(nome) {
    dados_tmp11 <- df$tmp11 %>%
      filter(Scenario == nome) %>%
      # arrange(year) %>%
      ungroup()

    dados_tmp11b <- df$tmp11b %>%
      filter(Scenario == nome)

    dados_k.out <- df$k.out %>%
      filter(Scenario == nome)

    plot_ly() %>%
      add_trace(
        data = dados_k.out %>% filter(q == "95%"),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255,0,0,0.2)",
        line = list(color = "gray30"),
        name = "95%"#,
        # hoverinfo = "none"
      ) %>%
      add_trace(
        data = dados_k.out %>% filter(q == "80%"),
        x = ~x, y = ~y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255,0,0,0.4)",
        line = list(color = "gray30"),
        name = "80%"
      ) %>%
      add_trace(
        data = dados_k.out %>% filter(q == "50%"),
        x = ~x, y = ~y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255,0,0,0.6)",
        line = list(color = "gray30"),
        name = "50%"
      ) %>%
      add_trace(
        data = dados_tmp11,
        x = ~Bratio,
        y = ~Fratio,
        type = "scatter",
        mode = "lines",
        line = list(color = "black", width = 1),
        name = "Trajectories",
        hoverinfo = "text",
        text = ~paste0(
          "Year: ", year, "<br>B/B<sub>MSY</sub>: ", mil_milhao(Bratio), "<br>F/F<sub>MSY</sub>: ", mil_milhao(Fratio)
        )
      ) %>%
      add_markers(
        data = dados_tmp11b,
        x = ~Bratio,
        y = ~Fratio,
        symbol = ~factor(year),
        size = 10,
        marker = list(
          color = "white",
          line = list(color = "black")
        ),
        name = "Years",
        hoverinfo = "text",
        text = ~paste0(
          "Year: ", year, "<br>B/B<sub>MSY</sub>: ", mil_milhao(Bratio), "<br>F/F<sub>MSY</sub>: ", mil_milhao(Fratio)
        )
      ) %>%
      layout(
        separators = ".",
        title = nome,
        shapes = list(
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df$col01$ymin,
            y1 = df$col01$ymax,
            x0 = df$col01$xmin,
            x1 = df$col01$xmax,
            fillcolor = df$col01$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df$col02$ymin,
            y1 = df$col02$ymax,
            x0 = df$col02$xmin,
            x1 = df$col02$xmax,
            fillcolor = df$col02$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df$col03$ymin,
            y1 = df$col03$ymax,
            x0 = df$col03$xmin,
            x1 = df$col03$xmax,
            fillcolor = df$col03$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df$col04$ymin,
            y1 = df$col04$ymax,
            x0 = df$col04$xmin,
            x1 = df$col04$xmax,
            fillcolor = "#00FF00",
            layer = "below"
          )
        ),
        xaxis = list(
          title = "B/B<sub>MSY</sub>",
          showgrid = FALSE,
          range = c(0, 4)
        ),
        yaxis = list(
          title = "F/F<sub>MSY</sub>",
          showgrid = FALSE,
          range = c(0, 3)
        ),
        showlegend = FALSE
      )
  })
  return(lista_plots_plotly)
}