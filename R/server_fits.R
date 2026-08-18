#' @keywords internal
.fits_server <- function(input, output, session, fits_df) { 
  filtered_fits <- reactiveVal(fits_df)

  title_x_fits <- reactiveVal(NULL)

  title_y_fits <- reactiveVal(NULL)

  palette_fits <- reactiveVal(NULL)

  x_lim_min_fits <- reactiveVal(NULL)

  x_lim_max_fits <- reactiveVal(NULL)

  y_lim_min_fits <- reactiveVal(NULL)

  y_lim_max_fits <- reactiveVal(NULL)

  fits_change <- reactiveValues(
    scenarios_changed = FALSE,
    indices_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    color_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  fits_values <- reactiveValues(
    scenarios_current = unique(fits_df$Scenario),
    indices_current = unique(fits_df$Index),
    title_x_current = NA,
    title_y_current = NA,
    color_current = "#1B4F8A",
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$fits_scenarios, {
    if (!setequal(input$fits_scenarios, fits_values$scenarios_current)) {
      fits_change$scenarios_changed = TRUE
    }
    else {
      fits_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_indices, {
    if (!setequal(input$fits_indices, fits_values$indices_current)) {
      fits_change$indices_changed = TRUE
    }
    else {
      fits_change$indices_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_title_x, {
    if (!identical(input$fits_title_x, fits_values$title_x_current)) {
      fits_change$title_x_changed = TRUE
    }
    else {
      fits_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_title_y, {
    if (!identical(input$fits_title_y, fits_values$title_y_current)) {
      fits_change$title_y_changed = TRUE
    }
    else {
      fits_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_color, {
    if (!identical(input$fits_color, fits_values$color_current)) {
      fits_change$color_changed = TRUE
    }
    else {
      fits_change$color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_x_min, {
    if (!identical(input$fits_x_min, fits_values$x_min_current)) {
      fits_change$x_min_changed = TRUE
    }
    else {
      fits_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_x_max, {
    if (!identical(input$fits_x_max, fits_values$x_max_current)) {
      fits_change$x_max_changed = TRUE
    }
    else {
      fits_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_y_min, {
    if (!identical(input$fits_y_min, fits_values$y_min_current)) {
      fits_change$y_min_changed = TRUE
    }
    else {
      fits_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fits_y_max, {
    if (!identical(input$fits_y_max, fits_values$y_max_current)) {
      fits_change$y_max_changed = TRUE
    }
    else {
      fits_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_fits <- reactive({
    req(input$navmenu == "tab_fits")
    vec <- unlist(reactiveValuesToList(fits_change))

    empty_condition <- .is_empty(input$fits_scenarios)|| 
      .is_empty(input$fits_indices)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_fits(), {
    if (status_sliders_fits()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_fits") {
      fits_values$scenarios_current = input$fits_scenarios
      fits_values$indices_current = input$fits_indices
      fits_values$title_x_current = input$fits_title_x
      fits_values$title_y_current = input$fits_title_y
      fits_values$color_current = input$fits_color
      fits_values$x_min_current = input$fits_x_min
      fits_values$x_max_current = input$fits_x_max
      fits_values$y_min_current = input$fits_y_min
      fits_values$y_max_current = input$fits_y_max

      fits_change$scenarios_changed = FALSE
      fits_change$indices_changed = FALSE
      fits_change$title_x_changed = FALSE
      fits_change$title_y_changed = FALSE
      fits_change$color_changed = FALSE
      fits_change$x_min_changed = FALSE
      fits_change$x_max_changed = FALSE
      fits_change$y_min_changed = FALSE
      fits_change$y_max_changed = FALSE

      filtered_fits(
        fits_df %>%
          filter(
            Scenario %in% input$fits_scenarios,
            Index %in% input$fits_indices
          ) %>%
          droplevels()
      )
      title_x_fits(input$fits_title_x)
      title_y_fits(input$fits_title_y)
      palette_fits(input$fits_color)
      x_lim_min_fits(input$fits_x_min)
      x_lim_max_fits(input$fits_x_max)
      y_lim_min_fits(input$fits_y_min)
      y_lim_max_fits(input$fits_y_max)
    }
  }, ignoreInit = TRUE)

  output$fits <- renderPlotly({
    req(filtered_fits())
    if (nrow(filtered_fits()) == 0) {
      return(.empty_plotly("There is no data for this plot"))
    }

    palette <- .resolve_palette(palette_fits(), 1)

    df <- filtered_fits()

    x_lim_min <- .get_value_or_default(
      x_lim_min_fits, min(df$Year, na.rm = TRUE)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_fits, max(df$Year, na.rm = TRUE)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_fits, .round_to_nearest(min(df$lci_95, na.rm = TRUE), FALSE)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_fits, .round_to_nearest(max(df$uci_95, na.rm = TRUE), TRUE)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df$Scenario)
    indices <- levels(df$Index)

    n_scenarios <- length(scenarios)
    n_indices <- length(indices)

    title_x <- .get_value_or_default(title_x_fits, "Year")

    title_y <- .get_value_or_default(title_y_fits, "Abundance index")

    plots <- map(scenarios, function(s) {
      map(indices, function(i) {

        df <- df %>%
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
        
        plot_ly() %>%
        add_ribbons(
          data = filter(df, !is.na(lci_95), !is.na(uci_95)),
          x = ~Year,
          ymin = ~lci_95,
          ymax = ~uci_95,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(95%): ", 
            .international_system_prefixes(lci_95, 2), "-", 
            .international_system_prefixes(uci_95, 2)
          )
        ) %>%
        add_ribbons(
          data = filter(df, !is.na(lci_80), !is.na(uci_80)),
          x = ~Year,
          ymin = ~lci_80,
          ymax = ~uci_80,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(80%): ", 
            .international_system_prefixes(lci_80, 2), "-", 
            .international_system_prefixes(uci_80, 2)  
          )
        ) %>%
        add_lines(
          data = filter(df, !is.na(mu_80)),
          x = ~Year,
          y = ~mu_80,
          type = "scatter",
          mode = "lines",
          line = list(width = 2, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0(
            "Mean: ", .international_system_prefixes(mu_80, 2)
          )
        ) %>%
        add_markers(
          data = filter(df, !is.na(Mean)),
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
          hoverinfo = "text+x",
          text = ~paste0(
            "Point: ", .international_system_prefixes(Mean, 2), 
            "<br>Interval: ", .international_system_prefixes(Li, 2), "-", 
            .international_system_prefixes(Ui, 2)
          )
        ) %>%
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
      })
    }) %>% flatten()

    results <- subplot(
      plots, 
      nrows = length(scenarios),
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
    # toc()
    results
  })
}