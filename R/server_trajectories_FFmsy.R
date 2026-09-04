#' @keywords internal
.traj_FFmsy_server <- function(
  input, output, session, traj_df, animation, use_si_suffix
) { 
  filtered_traj_FFmsy <- reactiveVal(traj_df)

  title_x_traj_FFmsy <- reactiveVal(NULL)

  title_y_traj_FFmsy <- reactiveVal(NULL)

  palette_traj_FFmsy <- reactiveVal(NULL)

  x_lim_min_traj_FFmsy <- reactiveVal(NULL)

  x_lim_max_traj_FFmsy <- reactiveVal(NULL)

  y_lim_min_traj_FFmsy <- reactiveVal(NULL)

  y_lim_max_traj_FFmsy <- reactiveVal(NULL)

  si_suffix_traj_FFmsy <- reactiveVal(use_si_suffix)

  traj_FFmsy_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    color_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE,
    si_suffix_changed = FALSE
  )

  traj_FFmsy_values <- reactiveValues(
    scenarios_current = unique(traj_df$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    color_current = "#1B4F8A",
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA,
    si_suffix_current = use_si_suffix
  )

  observeEvent(input$traj_FFmsy_scenarios, {
    if (!setequal(input$traj_FFmsy_scenarios, 
      traj_FFmsy_values$scenarios_current)) {
      traj_FFmsy_change$scenarios_changed = TRUE
    }
    else {
      traj_FFmsy_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_title_x, {
    if(!identical(input$traj_FFmsy_title_x, traj_FFmsy_values$title_x_current)){
      traj_FFmsy_change$title_x_changed = TRUE
    }
    else {
      traj_FFmsy_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_title_y, {
    if(!identical(input$traj_FFmsy_title_y, traj_FFmsy_values$title_y_current)){
      traj_FFmsy_change$title_y_changed = TRUE
    }
    else {
      traj_FFmsy_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_color, {
    if (!identical(input$traj_FFmsy_color, traj_FFmsy_values$color_current)) {
      traj_FFmsy_change$color_changed = TRUE
    }
    else {
      traj_FFmsy_change$color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_x_min, {
    if (!identical(input$traj_FFmsy_x_min, traj_FFmsy_values$x_min_current)) {
      traj_FFmsy_change$x_min_changed = TRUE
    }
    else {
      traj_FFmsy_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_x_max, {
    if (!identical(input$traj_FFmsy_x_max, traj_FFmsy_values$x_max_current)) {
      traj_FFmsy_change$x_max_changed = TRUE
    }
    else {
      traj_FFmsy_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_y_min, {
    if (!identical(input$traj_FFmsy_y_min, traj_FFmsy_values$y_min_current)) {
      traj_FFmsy_change$y_min_changed = TRUE
    }
    else {
      traj_FFmsy_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_y_max, {
    if (!identical(input$traj_FFmsy_y_max, traj_FFmsy_values$y_max_current)) {
      traj_FFmsy_change$y_max_changed = TRUE
    }
    else {
      traj_FFmsy_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$traj_FFmsy_si_suffix, {
    if (
      !identical(
        input$traj_FFmsy_si_suffix, traj_FFmsy_values$si_suffix_current
      )
    ) {
      traj_FFmsy_change$si_suffix_changed = TRUE
    }
    else {
      traj_FFmsy_change$si_suffix_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_traj_FFmsy <- reactive({
    req(input$navmenu == "tab_trajectories" && 
      input$trajectories_tabs == "tab_traj_FFmsy")
    vec <- unlist(reactiveValuesToList(traj_FFmsy_change))
    
    enable <- any(vec) && !.is_empty(input$traj_FFmsy_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_traj_FFmsy(), {
    if (status_sliders_traj_FFmsy()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (
      input$navmenu == "tab_trajectories" && 
      input$trajectories_tabs == "tab_traj_FFmsy"
    ) {
      y_min <- input$traj_FFmsy_y_min
      y_max <- input$traj_FFmsy_y_max

      if (!is.na(y_min) && !is.na(y_max) && y_min > y_max) {
        tmp_y <- y_min
        y_min <- y_max
        y_max <- tmp_y
        updateSelectInput(
          session, inputId = "traj_FFmsy_y_min", selected = y_min
        )
        updateSelectInput(
          session, inputId = "traj_FFmsy_y_max", selected = y_max
        )
        showNotification(
          ui = "First y value shouldn't be higher than the second y value",
          type = "warning", duration = 10
        )
      }
      
      x_min <- input$traj_FFmsy_x_min
      x_max <- input$traj_FFmsy_x_max

      if (!is.na(x_min) && !is.na(x_max) && x_min > x_max) {
        tmp_x <- x_min
        x_min <- x_max
        x_max <- tmp_x
        updateSelectInput(
          session, inputId = "traj_FFmsy_x_min", selected = x_min
        )
        updateSelectInput(
          session, inputId = "traj_FFmsy_x_max", selected = x_max
        )
        showNotification(
          ui = "First x value shouldn't be higher than the second x value",
          type = "warning", duration = 10
        )
      }

      traj_FFmsy_values$scenarios_current = input$traj_FFmsy_scenarios
      traj_FFmsy_values$title_x_current = input$traj_FFmsy_title_x
      traj_FFmsy_values$title_y_current = input$traj_FFmsy_title_y
      traj_FFmsy_values$color_current = input$traj_FFmsy_color
      traj_FFmsy_values$x_min_current = x_min
      traj_FFmsy_values$x_max_current = x_max
      traj_FFmsy_values$y_min_current = y_min
      traj_FFmsy_values$y_max_current = y_max
      traj_FFmsy_values$si_suffix_current = input$traj_FFmsy_si_suffix

      traj_FFmsy_change$scenarios_changed = FALSE
      traj_FFmsy_change$title_x_changed = FALSE
      traj_FFmsy_change$title_y_changed = FALSE
      traj_FFmsy_change$color_changed = FALSE
      traj_FFmsy_change$x_min_changed = FALSE
      traj_FFmsy_change$x_max_changed = FALSE
      traj_FFmsy_change$y_min_changed = FALSE
      traj_FFmsy_change$y_max_changed = FALSE
      traj_FFmsy_change$si_suffix_changed = FALSE

      filtered_traj_FFmsy(
        traj_df %>%
          filter(Scenario %in% input$traj_FFmsy_scenarios) %>%
          droplevels()
      )
      title_x_traj_FFmsy(input$traj_FFmsy_title_x)
      title_y_traj_FFmsy(input$traj_FFmsy_title_y)
      palette_traj_FFmsy(input$traj_FFmsy_color)
      x_lim_min_traj_FFmsy(x_min)
      x_lim_max_traj_FFmsy(x_max)
      y_lim_min_traj_FFmsy(y_min)
      y_lim_max_traj_FFmsy(y_max)
      si_suffix_traj_FFmsy(input$traj_FFmsy_si_suffix)
    }
  }, ignoreInit = TRUE)

  output$trajectories_FFmsy <- renderPlotly({
    req(filtered_traj_FFmsy())
    if (identical(filtered_traj_FFmsy(), data.frame())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- filtered_traj_FFmsy() %>%
      filter(indicator == "FFmsy")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(palette_traj_FFmsy(), 1)

    min_x <- min(df$year, na.rm = TRUE)
    max_x <- max(df$year, na.rm = TRUE)
    range <- max_x - min_x

    x_lim_min <- .get_value_or_default(x_lim_min_traj_FFmsy, min_x)
    x_lim_max <- .get_value_or_default(x_lim_max_traj_FFmsy, max_x)
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_traj_FFmsy, 
      .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_traj_FFmsy, 
      .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_traj_FFmsy, "Year")

    title_y <- .get_value_or_default(title_y_traj_FFmsy, "F/Fmsy")

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
            "CI(90): (", 
            .international_system_prefixes(lcl2, si_suffix_traj_FFmsy()), 
            ") - (", 
            .international_system_prefixes(ucl2, si_suffix_traj_FFmsy()), ")"
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
            "CI(97,5): (", 
            .international_system_prefixes(lcl, si_suffix_traj_FFmsy()), 
            ") - (", 
            .international_system_prefixes(ucl, si_suffix_traj_FFmsy()), ")"
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
          text = ~paste0(
            "Value: ", 
            .international_system_prefixes(mu, si_suffix_traj_FFmsy())
          )
        ) %>%
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
          hoverinfo = "none"
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