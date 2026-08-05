#' @keywords internal
.retrospective_analysis_MSY_server <- function(input, output, session, ra_df) {
  filtered_ra_MSY <- reactiveVal({ra_df})

  title_x_ra_MSY <- reactiveVal({NULL})

  title_y_ra_MSY <- reactiveVal({NULL})

  text_size_ra_MSY <- reactiveVal({16})

  x_lim_min_ra_MSY <- reactiveVal({NULL})

  x_lim_max_ra_MSY <- reactiveVal({NULL})

  y_lim_min_ra_MSY <- reactiveVal({NULL})

  y_lim_max_ra_MSY <- reactiveVal({NULL})

  ra_MSY_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  ra_MSY_values <- reactiveValues(
    scenarios_current = unique(ra_df$data$Scenario),
    title_x_current = NA,
    title_y_current = NA,
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$ra_MSY_scenarios, {
    if (!setequal(input$ra_MSY_scenarios, ra_MSY_values$scenarios_current)) {
      ra_MSY_change$scenarios_changed = TRUE
    }
    else {
      ra_MSY_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_title_x, {
    if (!identical(input$ra_MSY_title_x, ra_MSY_values$title_x_current)) {
      ra_MSY_change$title_x_changed = TRUE
    }
    else {
      ra_MSY_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_title_y, {
    if (!identical(input$ra_MSY_title_y, ra_MSY_values$title_y_current)) {
      ra_MSY_change$title_y_changed = TRUE
    }
    else {
      ra_MSY_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_text_size, {
    if (!identical(input$ra_MSY_text_size, ra_MSY_values$text_size_current)) {
      ra_MSY_change$text_size_changed = TRUE
    }
    else {
      ra_MSY_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_x_min, {
    if (!identical(input$ra_MSY_x_min, ra_MSY_values$x_min_current)) {
      ra_MSY_change$x_min_changed = TRUE
    }
    else {
      ra_MSY_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_x_max, {
    if (!identical(input$ra_MSY_x_max, ra_MSY_values$x_max_current)) {
      ra_MSY_change$x_max_changed = TRUE
    }
    else {
      ra_MSY_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_y_min, {
    if (!identical(input$ra_MSY_y_min, ra_MSY_values$y_min_current)) {
      ra_MSY_change$y_min_changed = TRUE
    }
    else {
      ra_MSY_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ra_MSY_y_max, {
    if (!identical(input$ra_MSY_y_max, ra_MSY_values$y_max_current)) {
      ra_MSY_change$y_max_changed = TRUE
    }
    else {
      ra_MSY_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_ra_MSY <- reactive({
    vec <- unlist(reactiveValuesToList(ra_MSY_change))
    
    enable <- any(vec) && !.is_empty(input$ra_MSY_scenarios)

    return(enable)
  })

  observeEvent(status_sliders_ra_MSY(), {

    if (status_sliders_ra_MSY()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  })

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_retrospective_analysis" && input$retrospective_analysis_tabs == "tab_ra_MSY") {
      ra_MSY_values$scenarios_current = input$ra_MSY_scenarios
      ra_MSY_values$indices_current = input$ra_MSY_indices
      ra_MSY_values$title_x_current = input$ra_MSY_title_x
      ra_MSY_values$title_y_current = input$ra_MSY_title_y
      ra_MSY_values$text_size = input$ra_MSY_text_size
      ra_MSY_values$x_min_current = input$ra_MSY_x_min
      ra_MSY_values$x_max_current = input$ra_MSY_x_max
      ra_MSY_values$y_min_current = input$ra_MSY_y_min
      ra_MSY_values$y_max_current = input$ra_MSY_y_max

      ra_MSY_change$scenarios_changed = FALSE
      ra_MSY_change$indices_changed = FALSE
      ra_MSY_change$title_x_changed = FALSE
      ra_MSY_change$title_y_changed = FALSE
      ra_MSY_change$text_size_changed = FALSE
      ra_MSY_change$x_min_changed = FALSE
      ra_MSY_change$x_max_changed = FALSE
      ra_MSY_change$y_min_changed = FALSE
      ra_MSY_change$y_max_changed = FALSE

      filtered_ra_MSY(
        list(
          surplus_data = ra_df$surplus_data %>%
            filter(
              Scenario %in% input$ra_MSY_scenarios
            ),
          rho_data = ra_df$rho_data %>%
            filter(
              Scenario %in% input$ra_MSY_scenarios
            )
        )
      )

      title_x_ra_MSY(input$ra_MSY_title_x)
      title_y_ra_MSY(input$ra_MSY_title_y)
      text_size_ra_MSY(input$ra_MSY_text_size)
      x_lim_min_ra_MSY(input$ra_MSY_x_min)
      x_lim_max_ra_MSY(input$ra_MSY_x_max)
      y_lim_min_ra_MSY(input$ra_MSY_y_min)
      y_lim_max_ra_MSY(input$ra_MSY_y_max)
    }
  })

  output$retrospective_analysis_MSY <- renderPlotly({
    if (identical(filtered_ra_MSY(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- filtered_ra_MSY()$surplus_data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    rho_data <- filtered_ra_MSY()$rho_data
  
    data_var <- data %>%
      filter(Index == "MSY")

    data_ref <- data_var %>%
      filter(id == "Ref")

    # data_ref <- data_var[data_var$id == "Ref", ]

    data_lines <- data_var

    rho_var <- rho_data %>% 
      filter(Index == "MSY")
    # rho_var <- rho_data[rho_data$Index == "MSY", ]

    # max_y_val <- .round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1)
    # min_y_val <- .round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1)
    # y_lim <- c(min_y_val, max_y_val)

    # max_x_val <- max(max(data_ref$SB_i), max(data_var$SB_i))
    # min_x_val <- min(min(data_ref$SB_i), min(data_var$SB_i))
    # x_lim <- c(min_x_val, max_x_val)

    if (is.null(x_lim_min_ra_MSY()) || x_lim_min_ra_MSY() == "" || is.na(x_lim_min_ra_MSY())) {
      x_lim_min_ra_MSY(min(data_ref$SB_i, data_var$SB_i))
    }
    if (is.null(x_lim_max_ra_MSY()) || x_lim_max_ra_MSY() == "" || is.na(x_lim_max_ra_MSY())) {
      x_lim_max_ra_MSY(max(data_ref$SB_i, data_var$SB_i))
    }
    x_lim <- c(x_lim_min_ra_MSY(), x_lim_max_ra_MSY())

    if (is.null(y_lim_min_ra_MSY()) || y_lim_min_ra_MSY() == "" || is.na(y_lim_min_ra_MSY())) {
      y_lim_min_ra_MSY(.round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1))
    }
    if (is.null(y_lim_max_ra_MSY()) || y_lim_max_ra_MSY() == "" || is.na(y_lim_max_ra_MSY())) {
      y_lim_max_ra_MSY(.round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1))
    }
    y_lim <- c(y_lim_min_ra_MSY(), y_lim_max_ra_MSY())

    if(is.null(title_x_ra_MSY()) || title_x_ra_MSY() == "") {
      title_x_ra_MSY("Year")
    }

    if (is.null(title_y_ra_MSY()) || title_y_ra_MSY() == "") {
      title_y_ra_MSY("Process error on log(Biomass)")
    }

    x_lim <- .expand_range(x_lim)
  
    pos <- .auto_text_position(
      data_list = data_lines,
      col_x = "SB_i",
      col_y = "SP",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.2
    )

    plots <- map(scenarios, function(s) {
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

      # data_lines <- data_lines[!is.na(data_lines$SB_i) & !is.na(data_lines$SP), ]

      data_lines <- data_lines %>%
        filter(!is.na(SB_i), !is.na(SP))

      plot_ly(colors = c("black", ss3col(8))) %>%
        add_lines(
          data = data_lines,
          x = ~SB_i,
          y = ~SP,
          color = ~as.factor(id),
          type = "scatter",
          mode = "lines",
          line = list(width = 3),
          hoverinfo = "text",
          text = ~paste0(
            "Biomass (", id,"): ", .international_system_prefixes(SB_i, 2), 
            "t<br>Surplus Production (", id,"): ", 
            .international_system_prefixes(SP, 2), "t" 
          )
        ) %>%
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = text_size_ra_MSY()),
          textposition = "middle left",
          hoverinfo = "skip"
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
            text = title_x_ra_MSY(),
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
            text = title_y_ra_MSY(),
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })
}