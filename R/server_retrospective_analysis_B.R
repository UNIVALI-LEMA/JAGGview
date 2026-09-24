#' @keywords internal
.retrospective_analysis_B_server <- function(
  input, output, session, ra_df, use_si_suffix
) {
  filtered_ra_B <- reactiveVal(ra_df)

  title_x_ra_B <- reactiveVal("Year")

  title_y_ra_B <- reactiveVal("Biomass (t)")

  text_size_ra_B <- reactiveVal(16)

  x_lim_min_ra_B <- reactiveVal(
    min((ra_df$data %>% filter(Index == "B"))$Year)
  )

  x_lim_max_ra_B <- reactiveVal(
    max((ra_df$data %>% filter(Index == "B"))$Year)
  )

  y_lim_min_ra_B <- reactiveVal(
    .round_to_nearest(
      min((ra_df$data %>% filter(Index == "B"))$lci, na.rm = TRUE), FALSE, 1.1
    )
  )

  y_lim_max_ra_B <- reactiveVal(
    .round_to_nearest(
      max((ra_df$data %>% filter(Index == "B"))$uci, na.rm = TRUE), TRUE, 1.1
    )
  )

  position_ra_B <- reactiveVal("top-left")

  si_suffix_ra_B <- reactiveVal(use_si_suffix)

  ra_B_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE,
    position_changed = FALSE,
    si_suffix_changed = FALSE
  )

  ra_B_values <- reactiveValues(
    scenarios_current = unique(ra_df$data$Scenario),
    title_x_current = "Year",
    title_y_current = "Biomass (t)",
    text_size_current = 16,
    x_min_current = min((ra_df$data %>% filter(Index == "B"))$Year),
    x_max_current = max((ra_df$data %>% filter(Index == "B"))$Year),
    y_min_current = .round_to_nearest(
      min((ra_df$data %>% filter(Index == "B"))$lci, na.rm = TRUE), FALSE, 1.1
    ),
    y_max_current = .round_to_nearest(
      max((ra_df$data %>% filter(Index == "B"))$uci, na.rm = TRUE), TRUE, 1.1
    ),
    position_current = "top-left",
    si_suffix_current = use_si_suffix
  )

  observeEvent(input$ra_B_scenarios, {
    if (!setequal(input$ra_B_scenarios, ra_B_values$scenarios_current)) {
      ra_B_change$scenarios_changed = TRUE
    }
    else {
      ra_B_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_title_x, {
    if (!identical(input$ra_B_title_x, ra_B_values$title_x_current)) {
      ra_B_change$title_x_changed = TRUE
    }
    else {
      ra_B_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_title_y, {
    if (!identical(input$ra_B_title_y, ra_B_values$title_y_current)) {
      ra_B_change$title_y_changed = TRUE
    }
    else {
      ra_B_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_text_size, {
    if (input$ra_B_text_size != ra_B_values$text_size_current) {
      ra_B_change$text_size_changed = TRUE
    }
    else {
      ra_B_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_x_min, {
    if (input$ra_B_x_min != ra_B_values$x_min_current) {
      ra_B_change$x_min_changed = TRUE
    }
    else {
      ra_B_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_x_max, {
    if (input$ra_B_x_max != ra_B_values$x_max_current) {
      ra_B_change$x_max_changed = TRUE
    }
    else {
      ra_B_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_y_min, {
    if (
      !isTRUE(
        all.equal(input$ra_B_y_min, ra_B_values$y_min_current)
      )
    ) {
      ra_B_change$y_min_changed = TRUE
    }
    else {
      ra_B_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_y_max, {
    if (
      !isTRUE(
        all.equal(input$ra_B_y_max, ra_B_values$y_max_current)
      )
    ) {
      ra_B_change$y_max_changed = TRUE
    }
    else {
      ra_B_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_position, {
    if (!identical(input$ra_B_position, ra_B_values$position_current)) {
      ra_B_change$position_changed = TRUE
    }
    else {
      ra_B_change$position_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_B_si_suffix, {
    if (!identical(input$ra_B_si_suffix, ra_B_values$si_suffix_current)) {
      ra_B_change$si_suffix_changed = TRUE
    }
    else {
      ra_B_change$si_suffix_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_ra_B <- reactive({
    req(input$navmenu == "tab_retrospective_analysis" && 
      input$retrospective_analysis_tabs == "tab_ra_B")
    vec <- unlist(reactiveValuesToList(ra_B_change))
    
    enable <- any(vec) && !.is_empty(input$ra_B_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_ra_B(), {
    if (status_sliders_ra_B()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    if (
      input$navmenu == "tab_retrospective_analysis" && 
      input$retrospective_analysis_tabs == "tab_ra_B"
    ) {
      updateControlbar(id = "controlbar", session = session)
      
      y_min <- input$ra_B_y_min
      y_max <- input$ra_B_y_max

      if (!is.na(y_min) && !is.na(y_max) && y_min > y_max) {
        tmp_y <- y_min
        y_min <- y_max
        y_max <- tmp_y
        updateSelectInput(
          session, inputId = "ra_B_y_min", selected = y_min
        )
        updateSelectInput(
          session, inputId = "ra_B_y_max", selected = y_max
        )
        showNotification(
          ui = "First y value shouldn't be higher than the second y value",
          type = "warning", duration = 10
        )
      }
      
      x_min <- .validate_year(input$ra_B_x_min, "ra_B_x_min", session)
      x_max <- .validate_year(input$ra_B_x_max, "ra_B_x_max", session)

      if (!is.na(x_min) && !is.na(x_max) && x_min > x_max) {
        tmp_x <- x_min
        x_min <- x_max
        x_max <- tmp_x
        updateSelectInput(
          session, inputId = "ra_B_x_min", selected = x_min
        )
        updateSelectInput(
          session, inputId = "ra_B_x_max", selected = x_max
        )
        showNotification(
          ui = "First x value shouldn't be higher than the second x value",
          type = "warning", duration = 10
        )
      }

      ra_B_values$scenarios_current = input$ra_B_scenarios
      ra_B_values$indices_current = input$ra_B_indices
      ra_B_values$title_x_current = input$ra_B_title_x
      ra_B_values$title_y_current = input$ra_B_title_y
      ra_B_values$text_size = input$ra_B_text_size
      ra_B_values$x_min_current = x_min
      ra_B_values$x_max_current = x_max
      ra_B_values$y_min_current = y_min
      ra_B_values$y_max_current = y_max
      ra_B_values$position_current = input$ra_B_position
      ra_B_values$si_suffix_current = input$ra_B_si_suffix

      ra_B_change$scenarios_changed = FALSE
      ra_B_change$indices_changed = FALSE
      ra_B_change$title_x_changed = FALSE
      ra_B_change$title_y_changed = FALSE
      ra_B_change$text_size_changed = FALSE
      ra_B_change$x_min_changed = FALSE
      ra_B_change$x_max_changed = FALSE
      ra_B_change$y_min_changed = FALSE
      ra_B_change$y_max_changed = FALSE
      ra_B_change$position_changed = FALSE
      ra_B_change$si_suffix_changed = FALSE

      filtered_ra_B(
        list(
          data = ra_df$data %>%
            filter(
              Scenario %in% input$ra_B_scenarios
            ),
          rho_data = ra_df$rho_data %>%
            filter(
              Scenario %in% input$ra_B_scenarios
            )
        )
      )

      title_x_ra_B(input$ra_B_title_x)
      title_y_ra_B(input$ra_B_title_y)
      text_size_ra_B(input$ra_B_text_size)
      x_lim_min_ra_B(x_min)
      x_lim_max_ra_B(x_max)
      y_lim_min_ra_B(y_min)
      y_lim_max_ra_B(y_max)
      position_ra_B(input$ra_B_position)
      si_suffix_ra_B(input$ra_B_si_suffix)
    }
  }, ignoreInit = TRUE)

  output$retrospective_analysis_B <- renderPlotly({
    req(filtered_ra_B())
    if (identical(filtered_ra_B(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- filtered_ra_B()$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    rho_data <- filtered_ra_B()$rho_data
    
    data_var <- data %>%
      filter(Index == "B")

    data_ref <- data_var %>%
      filter(id == "Ref")
    
    data_lines <- data_var %>%
      filter(teste == TRUE)
    
    rho_var <- rho_data %>%
      filter(Index == "B")

    x_lim <- .expand_range(c(x_lim_min_ra_B(), x_lim_max_ra_B()))
    y_lim <- .expand_range(c(y_lim_min_ra_B(), y_lim_max_ra_B()))

    si_suffix <- si_suffix_ra_B()
    text_size <- text_size_ra_B()
    position <- position_ra_B()

    plots <- map(scenarios, function(s) {
      data_ref <- data_ref %>%
        filter(Scenario == s)

      data_lines <- data_lines %>%
        filter(Scenario == s)

      rho_var <- rho_var %>%
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

      table <- .build_metric_table(
        rho_var, text_size, 
        str_split_i(position, "-", 2),
        str_split_i(position, "-", 1), 
        "rho", "\u03c1", decimals = 3
      )

      shapes <- append(shapes, table$shapes)

      annotations <- append(annotations, table$annotations)

      plot_ly(colors = c("black", ss3col(8))) %>%
        add_ribbons(
          data = data_ref,
          x = ~Year,
          ymin = ~lci,
          ymax = ~uci,
          fillcolor = "rgba(182, 186, 187, 0.64)",
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(95): (", .international_system_prefixes(lci, si_suffix), 
            ") - (", .international_system_prefixes(uci, si_suffix), ")"
          )
        ) %>%
        add_lines(
          data = data_lines,
          x = ~Year,
          y = ~mu,
          color = ~as.factor(id),
          type = "scatter",
          mode = "lines",
          line = list(width = 3),
          hoverinfo = "text+x",
          text = ~paste0(
            "mu (", id,"): ", 
            .international_system_prefixes(mu, si_suffix)
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
            l = 70,
            r = 50
          ),
          shapes = shapes,
          annotations = annotations
        ) %>%
        .plotly_config("retrospective_analysis_B_plot")
    })
    

    results <- subplot(
      plots,
      nrows = nrow,
      shareX = TRUE, 
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE, 
      margin = c(0.005, 0.005, 0.035, 0.035)
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
            text = title_x_ra_B(),
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
            xshift = -35,
            xref = "paper",
            yref = "paper",
            text = title_y_ra_B(),
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