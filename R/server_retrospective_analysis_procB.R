#' @keywords internal
.retrospective_analysis_procB_server <- function(input, output, session, ra_df){
  filtered_ra_procB <- reactiveVal(ra_df)

  title_x_ra_procB <- reactiveVal(NULL)

  title_y_ra_procB <- reactiveVal(NULL)

  text_size_ra_procB <- reactiveVal(16)

  x_lim_min_ra_procB <- reactiveVal(NULL)

  x_lim_max_ra_procB <- reactiveVal(NULL)

  y_lim_min_ra_procB <- reactiveVal(NULL)

  y_lim_max_ra_procB <- reactiveVal(NULL)

  position_ra_procB <- reactiveVal("top-left")

  ra_procB_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE,
    position_changed = FALSE
  )

  ra_procB_values <- reactiveValues(
    scenarios_current = unique(ra_df$data$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA,
    position_current = "top-left"
  )

  observeEvent(input$ra_procB_scenarios, {
    if (!setequal(input$ra_procB_scenarios, ra_procB_values$scenarios_current)){
      ra_procB_change$scenarios_changed = TRUE
    }
    else {
      ra_procB_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_title_x, {
    if (!identical(input$ra_procB_title_x, ra_procB_values$title_x_current)) {
      ra_procB_change$title_x_changed = TRUE
    }
    else {
      ra_procB_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_title_y, {
    if (!identical(input$ra_procB_title_y, ra_procB_values$title_y_current)) {
      ra_procB_change$title_y_changed = TRUE
    }
    else {
      ra_procB_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_text_size, {
    if(!identical(input$ra_procB_text_size, ra_procB_values$text_size_current)){
      ra_procB_change$text_size_changed = TRUE
    }
    else {
      ra_procB_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_x_min, {
    if (!identical(input$ra_procB_x_min, ra_procB_values$x_min_current)) {
      ra_procB_change$x_min_changed = TRUE
    }
    else {
      ra_procB_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_x_max, {
    if (!identical(input$ra_procB_x_max, ra_procB_values$x_max_current)) {
      ra_procB_change$x_max_changed = TRUE
    }
    else {
      ra_procB_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_y_min, {
    if (!identical(input$ra_procB_y_min, ra_procB_values$y_min_current)) {
      ra_procB_change$y_min_changed = TRUE
    }
    else {
      ra_procB_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_y_max, {
    if (!identical(input$ra_procB_y_max, ra_procB_values$y_max_current)) {
      ra_procB_change$y_max_changed = TRUE
    }
    else {
      ra_procB_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_procB_position, {
    if (!identical(input$ra_procB_position, ra_procB_values$position_current)) {
      ra_procB_change$position_changed = TRUE
    }
    else {
      ra_procB_change$position_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_ra_procB <- reactive({
    vec <- unlist(reactiveValuesToList(ra_procB_change))
    
    enable <- any(vec) && !.is_empty(input$ra_procB_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_ra_procB(), {
    req(input$navmenu == "tab_retrospective_analysis" && 
      input$retrospective_analysis_tabs == "tab_ra_procB")
    if (status_sliders_ra_procB()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    y_min <- input$ra_procB_y_min
    y_max <- input$ra_procB_y_max

    if (!is.na(y_min) && !is.na(y_max) && y_min > y_max) {
      tmp_y <- y_min
      y_min <- y_max
      y_max <- tmp_y
      updateSelectInput(
        session, inputId = "ra_procB_y_min", selected = y_min
      )
      updateSelectInput(
        session, inputId = "ra_procB_y_max", selected = y_max
      )
      showNotification(
        ui = "First y value shouldn't be higher than the second y value",
        type = "warning", duration = 10
      )
    }
    
    x_min <- input$ra_procB_x_min
    x_max <- input$ra_procB_x_max

    if (!is.na(x_min) && !is.na(x_max) && x_min > x_max) {
      tmp_x <- x_min
      x_min <- x_max
      x_max <- tmp_x
      updateSelectInput(
        session, inputId = "ra_procB_x_min", selected = x_min
      )
      updateSelectInput(
        session, inputId = "ra_procB_x_max", selected = x_max
      )
      showNotification(
        ui = "First x value shouldn't be higher than the second x value",
        type = "warning", duration = 10
      )
    }

    if (input$navmenu == "tab_retrospective_analysis" && 
      input$retrospective_analysis_tabs == "tab_ra_procB") {
      ra_procB_values$scenarios_current = input$ra_procB_scenarios
      ra_procB_values$indices_current = input$ra_procB_indices
      ra_procB_values$title_x_current = input$ra_procB_title_x
      ra_procB_values$title_y_current = input$ra_procB_title_y
      ra_procB_values$text_size = input$ra_procB_text_size
      ra_procB_values$x_min_current = x_min
      ra_procB_values$x_max_current = x_max
      ra_procB_values$y_min_current = y_min
      ra_procB_values$y_max_current = y_max
      ra_procB_values$position_current = input$ra_procB_position

      ra_procB_change$scenarios_changed = FALSE
      ra_procB_change$indices_changed = FALSE
      ra_procB_change$title_x_changed = FALSE
      ra_procB_change$title_y_changed = FALSE
      ra_procB_change$text_size_changed = FALSE
      ra_procB_change$x_min_changed = FALSE
      ra_procB_change$x_max_changed = FALSE
      ra_procB_change$y_min_changed = FALSE
      ra_procB_change$y_max_changed = FALSE
      ra_procB_change$position_changed = FALSE

      filtered_ra_procB(
        list(
          data = ra_df$data %>%
            filter(
              Scenario %in% input$ra_procB_scenarios
            ),
          rho_data = ra_df$rho_data %>%
            filter(
              Scenario %in% input$ra_procB_scenarios
            )
        )
      )

      title_x_ra_procB(input$ra_procB_title_x)
      title_y_ra_procB(input$ra_procB_title_y)
      text_size_ra_procB(input$ra_procB_text_size)
      x_lim_min_ra_procB(x_min)
      x_lim_max_ra_procB(x_max)
      y_lim_min_ra_procB(y_min)
      y_lim_max_ra_procB(y_max)
      position_ra_procB(input$ra_procB_position)
    }
  }, ignoreInit = TRUE)

  output$retrospective_analysis_procB <- renderPlotly({
    req(filtered_ra_procB())
    if (identical(filtered_ra_procB(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- filtered_ra_procB()$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    rho_data <- filtered_ra_procB()$rho_data
    
    data_var <- data %>%
      filter(Index == "procB")

    data_ref <- data_var %>%
      filter(id == "Ref")
    
    data_lines <- data_var %>%
      filter(teste == TRUE)
    
    rho_var <- rho_data %>%
      filter(Index == "procB")

    x_lim_min <- .get_value_or_default(
      x_lim_min_ra_procB, min(data_ref$Year, data_var$Year)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_ra_procB, max(data_ref$Year, data_var$Year)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_ra_procB, 
      .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_ra_procB, 
      .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_ra_procB, "Year")

    title_y <- .get_value_or_default(
      title_y_ra_procB, 
      "Process error on log(Biomass)"
    )

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

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
      position <- position_ra_procB()

      table <- .build_metric_table(
        rho_var, text_size_ra_procB(), 
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
            "CI(95): (", .international_system_prefixes(lci, 2), 
            ") - (", .international_system_prefixes(uci, 2), ")"
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
            "mu (", id,"): ", .international_system_prefixes(mu, 2)
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