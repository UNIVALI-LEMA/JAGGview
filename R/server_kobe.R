#' @keywords internal
.kobe_server <- function(input, output, session, kobe_df) {
  filtered_kobe <- reactiveVal(kobe_df)

  title_x_kobe <- reactiveVal(NULL)

  title_y_kobe <- reactiveVal(NULL)

  x_lim_min_kobe <- reactiveVal(NULL)

  x_lim_max_kobe <- reactiveVal(NULL)

  y_lim_min_kobe <- reactiveVal(NULL)

  y_lim_max_kobe <- reactiveVal(NULL)

  kobe_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  kobe_values <- reactiveValues(
    scenarios_current = unique(kobe_df$cpue_residuals$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$kobe_scenarios, {
    if (!setequal(input$kobe_scenarios, kobe_values$scenarios_current)) {
      kobe_change$scenarios_changed = TRUE
    }
    else {
      kobe_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_title_x, {
    if (!identical(input$kobe_title_x, kobe_values$title_x_current)) {
      kobe_change$title_x_changed = TRUE
    }
    else {
      kobe_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_title_y, {
    if (!identical(input$kobe_title_y, kobe_values$title_y_current)) {
      kobe_change$title_y_changed = TRUE
    }
    else {
      kobe_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_text_size, {
    if (!identical(input$kobe_text_size, kobe_values$text_size_current)) {
      kobe_change$text_size_changed = TRUE
    }
    else {
      kobe_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_x_min, {
    if (!identical(input$kobe_x_min, kobe_values$x_min_current)) {
      kobe_change$x_min_changed = TRUE
    }
    else {
      kobe_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_x_max, {
    if (!identical(input$kobe_x_max, kobe_values$x_max_current)) {
      kobe_change$x_max_changed = TRUE
    }
    else {
      kobe_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_y_min, {
    if (!identical(input$kobe_y_min, kobe_values$y_min_current)) {
      kobe_change$y_min_changed = TRUE
    }
    else {
      kobe_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$kobe_y_max, {
    if (!identical(input$kobe_y_max, kobe_values$y_max_current)) {
      kobe_change$y_max_changed = TRUE
    }
    else {
      kobe_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_kobe <- reactive({
    vec <- unlist(reactiveValuesToList(kobe_change))
    
    enable <- any(vec) && !.is_empty(input$kobe_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_kobe(), {
    if (status_sliders_kobe()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_kobe") {
      kobe_values$scenarios_current = input$kobe_scenarios
      kobe_values$title_x_current = input$kobe_title_x
      kobe_values$title_y_current = input$kobe_title_y
      kobe_values$text_size_current = input$kobe_text_size
      kobe_values$x_min_current = input$kobe_x_min
      kobe_values$x_max_current = input$kobe_x_max
      kobe_values$y_min_current = input$kobe_y_min
      kobe_values$y_max_current = input$kobe_y_max

      kobe_change$scenarios_changed = FALSE
      kobe_change$title_x_changed = FALSE
      kobe_change$title_y_changed = FALSE
      kobe_change$text_size_changed = FALSE
      kobe_change$x_min_changed = FALSE
      kobe_change$x_max_changed = FALSE
      kobe_change$y_min_changed = FALSE
      kobe_change$y_max_changed = FALSE

      filtered_kobe(
        list(
          col01 = kobe_df$col01,
          col02 = kobe_df$col02,
          col03 = kobe_df$col03,
          col04 = kobe_df$col04,
          k.out = kobe_df$k.out %>%
            filter(
              Scenario %in% input$kobe_scenarios
            ),
          tmp11 = kobe_df$tmp11 %>%
            filter(
              Scenario %in% input$kobe_scenarios
            ),
          tmp11b = kobe_df$tmp11b %>%
            filter(
              Scenario %in% input$kobe_scenarios
            )
        )
      )
      title_x_kobe(input$kobe_title_x)
      title_y_kobe(input$kobe_title_y)
      x_lim_min_kobe(input$kobe_x_min)
      x_lim_max_kobe(input$kobe_x_max)
      y_lim_min_kobe(input$kobe_y_min)
      y_lim_max_kobe(input$kobe_y_max)
    }
  }, ignoreInit = TRUE)

  output$kobe <- renderPlotly({
    req(filtered_kobe())
    if (identical(filtered_kobe(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_kobe()

    x_lim_min <- .get_value_or_default(x_lim_min_kobe, 0)
    x_lim_max <- .get_value_or_default(x_lim_max_kobe, df_lists$col02$xmax)
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(y_lim_min_kobe, 0)
    y_lim_max <- .get_value_or_default(y_lim_max_kobe, df_lists$col02$ymax)
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_kobe, "B/Bmsy")

    title_y <- .get_value_or_default(title_y_kobe, "F/Fmsy")

    scenarios <- unique(df_lists$k.out$Scenario)

    n_levels <- length(unique(df_lists$k.out$q))

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- colorRampPalette(c("cornsilk4", "grey", "cornsilk2"))(n_levels)

    plots <- map(scenarios, function(s) {
      line_data <- df_lists$tmp11 %>%
        filter(Scenario == s)

      marker_data <- df_lists$tmp11b %>%
        filter(Scenario == s)

      ci_data <- df_lists$k.out %>%
        filter(Scenario == s)

      shapes <- list()

      annotations <- list()

      shapes <- append(
        shapes,
        list(
          list(
            type = "rect",
            xref = "paper",
            yref = "paper",
            x0 = 0,
            x1 = 1,
            y0 = 0, 
            y1 = 1,
            line = list(width = 1)
          ),
          list(
            type = "rect",
            xref = "paper",
            yref = "paper",
            x0 = 0,
            x1 = 1,
            yanchor = 1,
            y0 = 0, 
            y1 = 28,
            ysizemode = "pixel",
            line = list(width = 1),
            fillcolor = "black"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col01$ymin,
            y1 = df_lists$col01$ymax,
            x0 = df_lists$col01$xmin,
            x1 = df_lists$col01$xmax,
            fillcolor = df_lists$col01$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col02$ymin,
            y1 = df_lists$col02$ymax,
            x0 = df_lists$col02$xmin,
            x1 = df_lists$col02$xmax,
            fillcolor = df_lists$col02$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col03$ymin,
            y1 = df_lists$col03$ymax,
            x0 = df_lists$col03$xmin,
            x1 = df_lists$col03$xmax,
            fillcolor = df_lists$col03$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col04$ymin,
            y1 = df_lists$col04$ymax,
            x0 = df_lists$col04$xmin,
            x1 = df_lists$col04$xmax,
            fillcolor = "#00FF00",
            layer = "below"
          )
        )
      )

      annotations <- append(
        annotations,
        list(
          list(
            x = 0.5,
            y = 1,
            xanchor = "center",
            yanchor = "top",
            yshift = 25,
            xref = "paper",
            yref = "paper",
            text = s,
            showarrow = FALSE,
            font = list(
              size = 20,
              color = "white"
            )
          )
        )
      )

      plot_ly() %>%
        add_segments(
          x = x_lim[1],
          xend = x_lim[2],
          y = 1, 
          yend = 1,
          line = list(
            color = "black",
            width = 2,
            dash = "20px,10px"
          ),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_segments(
          x = 1,
          xend = 1,
          y = y_lim[1], 
          yend = y_lim[2],
          line = list(
            color = "black",
            width = 2,
            dash = "20px,10px"
          ),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "95%"),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[1],
          line = list(color = "black"),
          name = "95%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "80%"),
          x = ~x, 
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[2],
          line = list(color = "black"),
          name = "80%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "50%"),
          x = ~x, 
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[3],
          line = list(color = "black"),
          name = "50%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = line_data,
          x = ~Bratio,
          y = ~Fratio,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 1),
          name = "Trajectories",
          hoverinfo = "text",
          text = ~paste0(
            "Year: ", year, "<br>B/B<sub>MSY</sub>: ", 
            .international_system_prefixes(Bratio), "<br>F/F<sub>MSY</sub>: ", 
            .international_system_prefixes(Fratio)
          )
        ) %>%
        add_markers(
          data = marker_data,
          x = ~Bratio,
          y = ~Fratio,
          symbol = ~factor(year),
          size = 10,
          marker = list(
            color = "white",
            line = list(color = "black")
          ),
          name = "Years",
          hoverinfo = "none"
        ) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            tickfont = list(size = 16),
            title = list(font = list(size = 20)),
            range = x_lim,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            tickfont = list(size = 16),
            title = list(font = list(size = 20)),
            range = y_lim,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          hoverdistance = -1,
          hoverlabel = list(font = list(size = 12)),
          margin = list(
            b = 50,
            t = 60,
            l = 60,
            r = 50
          ),
          shapes = shapes,
          annotations = annotations
        )
    }) %>% flatten()
    

    results <- subplot(
      plots,
      nrows = nrow,
      shareX = TRUE, 
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE, 
      margin = 0.02
    ) %>%
      layout(
        annotations = list(
          list(
            x = 0.5,
            y = 0,
            xanchor = "center",
            yanchor = "top",
            yshift = -20,
            xref = "paper",
            yref = "paper",
            text = title_x,
            showarrow = FALSE,
            font = list(
              size = 20
            )
          ),
          list(
            x = 0,
            y = 0.5,
            textangle = -90,
            xanchor = "right",
            yanchor = "middle",
            xshift = -30,
            xref = "paper",
            yref = "paper",
            text = title_y,
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
    # toc()
    results
    })
}