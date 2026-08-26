#' @keywords internal
.traj_H_server <- function(input, output, session, traj_df, animation) { 
  filtered_traj_H <- reactiveVal(traj_df)

  title_x_traj_H <- reactiveVal(NULL)

  title_y_traj_H <- reactiveVal(NULL)

  palette_traj_H <- reactiveVal(NULL)

  x_lim_min_traj_H <- reactiveVal(NULL)

  x_lim_max_traj_H <- reactiveVal(NULL)

  y_lim_min_traj_H <- reactiveVal(NULL)

  y_lim_max_traj_H <- reactiveVal(NULL)

  traj_H_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    color_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  traj_H_values <- reactiveValues(
    scenarios_current = unique(traj_df$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    color_current = "#1B4F8A",
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$traj_H_scenarios, {
    if (!setequal(input$traj_H_scenarios, traj_H_values$scenarios_current)) {
      traj_H_change$scenarios_changed = TRUE
    }
    else {
      traj_H_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_title_x, {
    if (!identical(input$traj_H_title_x, traj_H_values$title_x_current)) {
      traj_H_change$title_x_changed = TRUE
    }
    else {
      traj_H_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_title_y, {
    if (!identical(input$traj_H_title_y, traj_H_values$title_y_current)) {
      traj_H_change$title_y_changed = TRUE
    }
    else {
      traj_H_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_color, {
    if (!identical(input$traj_H_color, traj_H_values$color_current)) {
      traj_H_change$color_changed = TRUE
    }
    else {
      traj_H_change$color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_x_min, {
    if (!identical(input$traj_H_x_min, traj_H_values$x_min_current)) {
      traj_H_change$x_min_changed = TRUE
    }
    else {
      traj_H_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_x_max, {
    if (!identical(input$traj_H_x_max, traj_H_values$x_max_current)) {
      traj_H_change$x_max_changed = TRUE
    }
    else {
      traj_H_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_y_min, {
    if (!identical(input$traj_H_y_min, traj_H_values$y_min_current)) {
      traj_H_change$y_min_changed = TRUE
    }
    else {
      traj_H_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_H_y_max, {
    if (!identical(input$traj_H_y_max, traj_H_values$y_max_current)) {
      traj_H_change$y_max_changed = TRUE
    }
    else {
      traj_H_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_traj_H <- reactive({
    req(input$navmenu == "tab_trajectories" && 
      input$trajectories_tabs == "tab_traj_H")
    vec <- unlist(reactiveValuesToList(traj_H_change))
    
    enable <- any(vec) && !.is_empty(input$traj_H_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_traj_H(), {
    if (status_sliders_traj_H()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_trajectories" && 
      input$trajectories_tabs == "tab_traj_H") {
      traj_H_values$scenarios_current = input$traj_H_scenarios
      traj_H_values$title_x_current = input$traj_H_title_x
      traj_H_values$title_y_current = input$traj_H_title_y
      traj_H_values$color_current = input$traj_H_color
      traj_H_values$x_min_current = input$traj_H_x_min
      traj_H_values$x_max_current = input$traj_H_x_max
      traj_H_values$y_min_current = input$traj_H_y_min
      traj_H_values$y_max_current = input$traj_H_y_max

      traj_H_change$scenarios_changed = FALSE
      traj_H_change$title_x_changed = FALSE
      traj_H_change$title_y_changed = FALSE
      traj_H_change$color_changed = FALSE
      traj_H_change$x_min_changed = FALSE
      traj_H_change$x_max_changed = FALSE
      traj_H_change$y_min_changed = FALSE
      traj_H_change$y_max_changed = FALSE

      filtered_traj_H(
        traj_df %>%
          filter(Scenario %in% input$traj_H_scenarios) %>%
          droplevels()
      )
      title_x_traj_H(input$traj_H_title_x)
      title_y_traj_H(input$traj_H_title_y)
      palette_traj_H(input$traj_H_color)
      x_lim_min_traj_H(input$traj_H_x_min)
      x_lim_max_traj_H(input$traj_H_x_max)
      y_lim_min_traj_H(input$traj_H_y_min)
      y_lim_max_traj_H(input$traj_H_y_max)
    }
  }, ignoreInit = TRUE)

  output$trajectories_H <- renderPlotly({
    req(filtered_traj_H())
    if (identical(filtered_traj_H(), data.frame())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- filtered_traj_H() %>%
      filter(indicator == "H")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(palette_traj_H(), 1)

    min_x <- min(df$year, na.rm = TRUE)
    max_x <- max(df$year, na.rm = TRUE)
    range <- max_x - min_x

    x_lim_min <- .get_value_or_default(x_lim_min_traj_H, min_x)
    x_lim_max <- .get_value_or_default(x_lim_max_traj_H, max_x)
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_traj_H, 
      .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_traj_H, 
      .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_traj_H, "Year")

    title_y <- .get_value_or_default(title_y_traj_H, "Harvest rate")

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    plots <- map(scenarios, function(s) {
      df <- df %>%
        filter(Scenario == s)
      if (animation) {
        df <- df %>%
        .accumulate_by(year, step = round(range / 25))
      }
        
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
          frame =  if (animation) ~frame else NULL,
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
          frame =  if (animation) ~frame else NULL,
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
          type = "scattergl",
          mode = "lines",
          line = list(width = 3, color = "black"),
          frame =  if (animation) ~frame else NULL,
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

    results <- subplot(
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
    if (animation) {
      results <- results %>%
        animation_slider(
          hide = TRUE
        ) %>%
        animation_button(
          visible = FALSE
        ) %>%
        onRender("
          function(el,x){
            Plotly.animate(el, null, {
              frame: {duration: 5, redraw: false},
              transition: {duration: 0}
            });
          }
        ")
    }
    # toc()
    results
    })
}