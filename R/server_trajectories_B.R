#' @keywords internal
.traj_B_server <- function(input, output, session, traj_df) { 
  filtered_traj_B <- reactiveVal({traj_df})

  title_x_traj_B <- reactiveVal({NULL})

  title_y_traj_B <- reactiveVal({NULL})

  palette_traj_B <- reactiveVal({NULL})

  x_lim_min_traj_B <- reactiveVal({NULL})

  x_lim_max_traj_B <- reactiveVal({NULL})

  y_lim_min_traj_B <- reactiveVal({NULL})

  y_lim_max_traj_B <- reactiveVal({NULL})

  traj_B_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    color_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  traj_B_values <- reactiveValues(
    scenarios_current = unique(traj_df$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    color_current = "#1B4F8A",
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$traj_B_scenarios, {
    if (!setequal(input$traj_B_scenarios, traj_B_values$scenarios_current)) {
      traj_B_change$scenarios_changed = TRUE
    }
    else {
      traj_B_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_title_x, {
    if (!identical(input$traj_B_title_x, traj_B_values$title_x_current)) {
      traj_B_change$title_x_changed = TRUE
    }
    else {
      traj_B_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_title_y, {
    if (!identical(input$traj_B_title_y, traj_B_values$title_y_current)) {
      traj_B_change$title_y_changed = TRUE
    }
    else {
      traj_B_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_color, {
    if (!identical(input$traj_B_color, traj_B_values$color_current)) {
      traj_B_change$color_changed = TRUE
    }
    else {
      traj_B_change$color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_x_min, {
    if (!identical(input$traj_B_x_min, traj_B_values$x_min_current)) {
      traj_B_change$x_min_changed = TRUE
    }
    else {
      traj_B_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_x_max, {
    if (!identical(input$traj_B_x_max, traj_B_values$x_max_current)) {
      traj_B_change$x_max_changed = TRUE
    }
    else {
      traj_B_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_y_min, {
    if (!identical(input$traj_B_y_min, traj_B_values$y_min_current)) {
      traj_B_change$y_min_changed = TRUE
    }
    else {
      traj_B_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_B_y_max, {
    if (!identical(input$traj_B_y_max, traj_B_values$y_max_current)) {
      traj_B_change$y_max_changed = TRUE
    }
    else {
      traj_B_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_traj_B <- reactive({
    vec <- unlist(reactiveValuesToList(traj_B_change))
    
    enable <- any(vec) && !.is_empty(input$traj_B_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_traj_B(), {

    if (status_sliders_traj_B()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  })

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_trajectories" && input$trajectories_tabs == "tab_traj_B") {
      traj_B_values$scenarios_current = input$traj_B_scenarios
      traj_B_values$title_x_current = input$traj_B_title_x
      traj_B_values$title_y_current = input$traj_B_title_y
      traj_B_values$color_current = input$traj_B_color
      traj_B_values$x_min_current = input$traj_B_x_min
      traj_B_values$x_max_current = input$traj_B_x_max
      traj_B_values$y_min_current = input$traj_B_y_min
      traj_B_values$y_max_current = input$traj_B_y_max

      traj_B_change$scenarios_changed = FALSE
      traj_B_change$title_x_changed = FALSE
      traj_B_change$title_y_changed = FALSE
      traj_B_change$color_changed = FALSE
      traj_B_change$x_min_changed = FALSE
      traj_B_change$x_max_changed = FALSE
      traj_B_change$y_min_changed = FALSE
      traj_B_change$y_max_changed = FALSE

      filtered_traj_B(
        traj_df %>%
          filter(
            Scenario %in% input$traj_B_scenarios
          ) %>%
          droplevels()
      )
      title_x_traj_B(input$traj_B_title_x)
      title_y_traj_B(input$traj_B_title_y)
      palette_traj_B(input$traj_B_color)
      x_lim_min_traj_B(input$traj_B_x_min)
      x_lim_max_traj_B(input$traj_B_x_max)
      y_lim_min_traj_B(input$traj_B_y_min)
      y_lim_max_traj_B(input$traj_B_y_max)
    }
  })

  output$trajectories_B <- renderPlotly({
    if (identical(filtered_traj_B(), data.frame())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- filtered_traj_B() %>%
      filter(indicator == "B")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(palette_traj_B(), 1)

    if (is.null(x_lim_min_traj_B()) || x_lim_min_traj_B() == "" || is.na(x_lim_min_traj_B())) {
      x_lim_min_traj_B(min(df$year, na.rm = TRUE))
    }

    if (is.null(x_lim_max_traj_B()) || x_lim_max_traj_B() == "" || is.na(x_lim_max_traj_B())) {
      x_lim_max_traj_B(max(df$year, na.rm = TRUE))
    }
    x_lim <- c(x_lim_min_traj_B(), x_lim_max_traj_B())

    if (is.null(y_lim_min_traj_B()) || y_lim_min_traj_B() == "" || is.na(y_lim_min_traj_B())) {
      y_lim_min_traj_B(.round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1))
    }

    if (is.null(y_lim_max_traj_B()) || y_lim_max_traj_B() == "" || is.na(y_lim_max_traj_B())) {
      y_lim_max_traj_B(.round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1))
    }
    y_lim <- c(y_lim_min_traj_B(), y_lim_max_traj_B())

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
  
    if(is.null(title_x_traj_B()) || title_x_traj_B() == "") {
      title_x_traj_B("Year")
    }

    if (is.null(title_y_traj_B()) || title_y_traj_B() == "") {
      title_y_traj_B("Biomass (t)")
    }

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(90): (", .international_system_prefixes(lcl2, 2), 
            ") - (", .international_system_prefixes(ucl2, 2), ")"
          )
        ) %>%
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl,
          ymax = ~ucl,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(97,5): (", .international_system_prefixes(lcl, 2), 
            ") - (", .international_system_prefixes(ucl, 2), ")"
          )
        ) %>%
        add_lines(
          data = df,
          x = ~year,
          y = ~mu,
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0("Value: ", .international_system_prefixes(mu, 2))
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
    }) %>%
      flatten()

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
            text = title_x_traj_B(),
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
            text = title_y_traj_B(),
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })
}