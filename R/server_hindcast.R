#' @keywords internal
.hindcast_server <- function(input, output, session, hind_df) { 
  filtered_hc <- reactiveVal(hind_df)

  title_x_hc <- reactiveVal(NULL)

  title_y_hc <- reactiveVal(NULL)

  text_size_hc <- reactiveVal(16)

  x_lim_min_hc <- reactiveVal(NULL)

  x_lim_max_hc <- reactiveVal(NULL)

  y_lim_min_hc <- reactiveVal(NULL)

  y_lim_max_hc <- reactiveVal(NULL)

  hc_change <- reactiveValues(
    scenarios_changed = FALSE,
    indices_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  hc_values <- reactiveValues(
    scenarios_current = unique(hind_df$data$Scenario),
    indices_current = unique(hind_df$data$Index),
    title_x_current = NA,
    title_y_current = NA,
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$hc_scenarios, {
    if (!setequal(input$hc_scenarios, hc_values$scenarios_current)) {
      hc_change$scenarios_changed = TRUE
    }
    else {
      hc_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_indices, {
    if (!setequal(input$hc_indices, hc_values$indices_current)) {
      hc_change$indices_changed = TRUE
    }
    else {
      hc_change$indices_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_title_x, {
    if (!identical(input$hc_title_x, hc_values$title_x_current)) {
      hc_change$title_x_changed = TRUE
    }
    else {
      hc_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_title_y, {
    if (!identical(input$hc_title_y, hc_values$title_y_current)) {
      hc_change$title_y_changed = TRUE
    }
    else {
      hc_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_text_size, {
    if (!identical(input$hc_text_size, hc_values$text_size_current)) {
      hc_change$text_size_changed = TRUE
    }
    else {
      hc_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_x_min, {
    if (!identical(input$hc_x_min, hc_values$x_min_current)) {
      hc_change$x_min_changed = TRUE
    }
    else {
      hc_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_x_max, {
    if (!identical(input$hc_x_max, hc_values$x_max_current)) {
      hc_change$x_max_changed = TRUE
    }
    else {
      hc_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_y_min, {
    if (!identical(input$hc_y_min, hc_values$y_min_current)) {
      hc_change$y_min_changed = TRUE
    }
    else {
      hc_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$hc_y_max, {
    if (!identical(input$hc_y_max, hc_values$y_max_current)) {
      hc_change$y_max_changed = TRUE
    }
    else {
      hc_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_hc <- reactive({
    vec <- unlist(reactiveValuesToList(hc_change))

    empty_condition <- .is_empty(input$hc_scenarios)|| .is_empty(input$hc_indices)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_hc(), {
    if (status_sliders_hc()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_hindcast") {
      hc_values$scenarios_current = input$hc_scenarios
      hc_values$indices_current = input$hc_indices
      hc_values$title_x_current = input$hc_title_x
      hc_values$title_y_current = input$hc_title_y
      hc_values$text_size_current = input$hc_text_size
      hc_values$x_min_current = input$hc_x_min
      hc_values$x_max_current = input$hc_x_max
      hc_values$y_min_current = input$hc_y_min
      hc_values$y_max_current = input$hc_y_max

      hc_change$scenarios_changed = FALSE
      hc_change$indices_changed = FALSE
      hc_change$title_x_changed = FALSE
      hc_change$title_y_changed = FALSE
      hc_change$text_size_changed = FALSE
      hc_change$x_min_changed = FALSE
      hc_change$x_max_changed = FALSE
      hc_change$y_min_changed = FALSE
      hc_change$y_max_changed = FALSE

      filtered_hc(
        list(
          data = hind_df$data %>%
            filter(
              Scenario %in% input$hc_scenarios,
              Index %in% input$hc_indices
            ) %>% droplevels(),
          hindcast_data_1 = hind_df$hindcast_data_1 %>%
            filter(
              Scenario %in% input$hc_scenarios,
              Index %in% input$hc_indices
            ) %>% droplevels(),
          hindcast_data_2 = hind_df$hindcast_data_2 %>%
            filter(
              Scenario %in% input$hc_scenarios,
              Index %in% input$hc_indices
            ) %>% droplevels(),
          mase_data = hind_df$mase_data %>%
            filter(
              Scenario %in% input$hc_scenarios,
              Index %in% input$hc_indices
            ) %>% droplevels(),
          min_year_retro = hind_df$min_year_retro
        )
      )
      title_x_hc(input$hc_title_x)
      title_y_hc(input$hc_title_y)
      text_size_hc(input$hc_text_size)
      x_lim_min_hc(input$hc_x_min)
      x_lim_max_hc(input$hc_x_max)
      y_lim_min_hc(input$hc_y_min)
      y_lim_max_hc(input$hc_y_max)
    }
  }, ignoreInit = TRUE)

  output$hindcast <- renderPlotly({
    req(filtered_hc())
    if (identical(filtered_hc(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_hc()

    scenarios <- unique(df_lists$data$Scenario)
    indices <- levels(df_lists$data$Index)
    
    n_scenarios <- length(scenarios)
    n_indices <- length(indices)
    
    nrow <- case_when(
      n_scenarios > n_indices ~ n_scenarios,
      n_scenarios < 3          ~ 1,
      n_scenarios < 8          ~ 2,
      TRUE                     ~ 3
    )
  
    # if (is.null(x_lim_min_hc()) || x_lim_min_hc() == "" || is.na(x_lim_min_hc())) {
    #   x_lim_min_hc(min(df_lists$data$year))
    # }

    # if (is.null(x_lim_max_hc()) || x_lim_max_hc() == "" || is.na(x_lim_max_hc())) {
    #   x_lim_max_hc(max(df_lists$data$year))
    # }
    # x_lim <- c(x_lim_min_hc(), x_lim_max_hc())

    # if (is.null(y_lim_min_hc()) || y_lim_min_hc() == "" || is.na(y_lim_min_hc())) {
    #   y_lim_min_hc(.round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE))
    # }

    # if (is.null(y_lim_max_hc()) || y_lim_max_hc() == "" || is.na(y_lim_max_hc())) {
    #   y_lim_max_hc(.round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE))
    # }
    # y_lim <- c(y_lim_min_hc(), y_lim_max_hc())

    # y_lim <- .expand_range(y_lim)
    # x_lim <- .expand_range(x_lim)

    # if(is.null(title_x_hc()) || title_x_hc() == "") {
    #   title_x_hc("Year")
    # }

    # if (is.null(title_y_hc()) || title_y_hc() == "") {
    #   title_y_hc("Index")
    # }

    x_lim_min <- .get_value_or_default(
      x_lim_min_hc, min(df_lists$data$year)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_hc, max(df_lists$data$year)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_hc, 
      .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_hc, 
      .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    title_x <- .get_value_or_default(title_x_hc, "Year")

    title_y <- .get_value_or_default(title_y_hc, "Index")

    pos <- .auto_text_position(
      df_lists$data, 
      "year", 
      "hat.uci", 
      xlim = x_lim,
      ylim = y_lim
    )

    min_year_hc <- min(df_lists$hindcast_data_2$year) - 1

    max_year_hc <- max(df_lists$hindcast_data_2$year)
  
    plots <- map(scenarios, function(s) {
      map(indices, function(i) {
        data <- df_lists$data %>%
          filter(Scenario == s, Index == i)

        hc_data_1 <- df_lists$hindcast_data_1 %>%
          filter(Scenario == s, Index == i)

        hc_data_2 <- df_lists$hindcast_data_2 %>%
          filter(Scenario == s, Index == i)

        mase_data <- df_lists$mase_data %>%
          filter(Scenario == s, Index == i)

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
            )
          )
        )

        if (s == scenarios[1]) {
          shapes <- append(
            shapes,
            list(
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
                text = i,
                showarrow = FALSE,
                font = list(
                  size = 20,
                  color = "white"
                )
              )
            )
          )
        }

        if (i == indices[n_indices]) {
          shapes <- append(
            shapes,
            list(
              list(
                type = "rect",
                xref = "paper",
                yref = "paper",
                xanchor = 1,
                x0 = 0,
                x1 = 28,
                y0 = 0, 
                y1 = 1,
                xsizemode = "pixel",
                line = list(width = 1),
                fillcolor = "black"
              )
            )
          )
          annotations <- append(
            annotations,
            list(
              list(
                x = 1,
                y = 0.5,
                textangle = 90,
                xanchor = "center",
                yanchor = "middle",
                xshift = 15,
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
        }

        p <- plot_ly(colors = c("black", ss3col(8))) %>%
          add_ribbons(
            data = filter(data, retro.peels == 0),
            x = ~year,
            ymin = ~hat.lci,
            ymax = ~hat.uci,
            fillcolor = alpha("gray", alpha = 0.4),
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat CI(95): (", .international_system_prefixes(hat.lci, 2), 
              ") - (", .international_system_prefixes(hat.uci, 2), ")"
            )
          ) %>%
          add_ribbons(
            data = filter(data, retro.peels == 0, 
              year < df_lists$min_year_retro),
            x = ~year,
            ymin = ~hat.lci,
            ymax = ~hat.uci,
            fillcolor = alpha("gray", alpha = 0.9),
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat CI(95): (", .international_system_prefixes(hat.lci, 2), 
              ") - (", .international_system_prefixes(hat.uci, 2), ")"
            )
          ) %>%
          add_lines(
            data = filter(data, hindcast == FALSE),
            x = ~year,
            y = ~hat,
            color = ~as.factor(retro),
            type = "scatter",
            mode = "lines",
            line = list(width = 2),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat (", retro,"): ", .international_system_prefixes(hat, 2)
            ),
            inherit = FALSE
          ) %>%
          add_lines(
            data = hc_data_2,
            x = ~year,
            y = ~hat,
            line = list(width = 2, color = "black"),
            split = ~retro.peels,
            hoverinfo = "text+x",
            text = ~paste0(
              "hat - hindcast(", retro,"): ", 
              .international_system_prefixes(hat, 2)
            )
          ) %>%
          add_markers(
            data = filter(data, retro.peels == 0, 
              year < df_lists$min_year_retro),
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
            hoverinfo = "text+x",
            text = ~paste0(
              "obs: ", .international_system_prefixes(obs, 2)
            )
          ) %>%
          add_markers(
            data = hc_data_1,
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
            hoverinfo = "text+x",
            text = ~paste0(
              "hindcast obs (", retro,"): ",
              .international_system_prefixes(obs, 2)
            )
          ) %>%
          add_markers(
            data = hc_data_1,
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
            hoverinfo = "text+x",
            text = ~paste0(
              "hindcast hat (", retro,"): ", 
              .international_system_prefixes(hat, 2)
            )
          ) 
        if (nrow(mase_data)) {
          p <- p %>%
            add_text(
              data = mase_data,
              x = pos$x,
              y = pos$y,
              hoverinfo = "none",
              textfont = list(size = text_size_hc()),
              text = ~paste0(
                "MASE = ", .international_system_prefixes(MASE, decimals = 3)
              )
            )
        }
        p <- p %>%
          layout(
            showlegend = FALSE,
            xaxis = list(
              tickfont = list(size = 16),
              title = list(font = list(size = 20)),
              range = x_lim,
              zeroline = FALSE
            ),
            yaxis = list(
              tickfont = list(size = 16),
              title = list(font = list(size = 20)),
              range = y_lim,
              zeroline = FALSE
            ),
            hovermode = "x unified",
            hoverdistance = 1,
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
        p
      })
    }) %>% flatten()

    subplot(
      plots, 
      nrows = nrow,
      shareX = TRUE, 
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE, 
      margin = 0.005
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
            xshift = -20,
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
  })
}