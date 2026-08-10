#' @keywords internal
.priors_posteriors_psi_server <- function(input, output, session, pp_df) {
  filtered_pp_psi <- reactiveVal(pp_df)

  title_x_pp_psi <- reactiveVal(NULL)

  title_y_pp_psi <- reactiveVal(NULL)

  prior_color_pp_psi <- reactiveVal(NULL)
  
  posterior_color_pp_psi <- reactiveVal(NULL)

  text_size_pp_psi <- reactiveVal(16)

  x_lim_min_pp_psi <- reactiveVal(NULL)

  x_lim_max_pp_psi <- reactiveVal(NULL)

  y_lim_min_pp_psi <- reactiveVal(NULL)

  y_lim_max_pp_psi <- reactiveVal(NULL)

  pp_psi_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    prior_color_changed = FALSE,
    posterior_color_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE
  )

  pp_psi_values <- reactiveValues(
    scenarios_current = unique(c(pp_df$prior$Scenario, 
      pp_df$posterior$Scenario)),
    title_x_current = NA,
    title_y_current = NA,
    prior_color_current = "#1B4F8A",
    posterior_color_current = "#2A9D5C",
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA
  )

  observeEvent(input$pp_psi_scenarios, {
    if (!setequal(input$pp_psi_scenarios, pp_psi_values$scenarios_current)) {
      pp_psi_change$scenarios_changed = TRUE
    }
    else {
      pp_psi_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_title_x, {
    if (!identical(input$pp_psi_title_x, pp_psi_values$title_x_current)) {
      pp_psi_change$title_x_changed = TRUE
    }
    else {
      pp_psi_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_title_y, {
    if (!identical(input$pp_psi_title_y, pp_psi_values$title_y_current)) {
      pp_psi_change$title_y_changed = TRUE
    }
    else {
      pp_psi_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_prior_color, {
    if(!identical(input$pp_psi_prior_color, pp_psi_values$prior_color_current)){
      pp_psi_change$prior_color_changed = TRUE
    }
    else {
      pp_psi_change$prior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_posterior_color, {
    if (!identical(input$pp_psi_posterior_color, 
      pp_psi_values$posterior_color_current)) {
      pp_psi_change$posterior_color_changed = TRUE
    }
    else {
      pp_psi_change$posterior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_text_size, {
    if (!identical(input$pp_psi_text_size, pp_psi_values$text_size_current)) {
      pp_psi_change$text_size_changed = TRUE
    }
    else {
      pp_psi_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_x_min, {
    if (!identical(input$pp_psi_x_min, pp_psi_values$x_min_current)) {
      pp_psi_change$x_min_changed = TRUE
    }
    else {
      pp_psi_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_x_max, {
    if (!identical(input$pp_psi_x_max, pp_psi_values$x_max_current)) {
      pp_psi_change$x_max_changed = TRUE
    }
    else {
      pp_psi_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_y_min, {
    if (!identical(input$pp_psi_y_min, pp_psi_values$y_min_current)) {
      pp_psi_change$y_min_changed = TRUE
    }
    else {
      pp_psi_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_psi_y_max, {
    if (!identical(input$pp_psi_y_max, pp_psi_values$y_max_current)) {
      pp_psi_change$y_max_changed = TRUE
    }
    else {
      pp_psi_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_pp_psi <- reactive({
    vec <- unlist(reactiveValuesToList(pp_psi_change))

    empty_condition <- .is_empty(input$pp_psi_scenarios)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_pp_psi(), {
    if (status_sliders_pp_psi()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_priors_posteriors" && 
      input$priors_posteriors_tabs == "tab_pp_psi") {
      pp_psi_values$scenarios_current = input$pp_psi_scenarios
      pp_psi_values$indices_current = input$pp_psi_indices
      pp_psi_values$title_x_current = input$pp_psi_title_x
      pp_psi_values$title_y_current = input$pp_psi_title_y
      pp_psi_values$prior_color_current = input$pp_psi_prior_color
      pp_psi_values$posterior_color_current = input$pp_psi_posterior_color
      pp_psi_values$text_size = input$pp_psi_text_size
      pp_psi_values$x_min_current = input$pp_psi_x_min
      pp_psi_values$x_max_current = input$pp_psi_x_max
      pp_psi_values$y_min_current = input$pp_psi_y_min
      pp_psi_values$y_max_current = input$pp_psi_y_max

      pp_psi_change$scenarios_changed = FALSE
      pp_psi_change$indices_changed = FALSE
      pp_psi_change$title_x_changed = FALSE
      pp_psi_change$title_y_changed = FALSE
      pp_psi_change$color_changed = FALSE
      pp_psi_change$text_size_changed = FALSE
      pp_psi_change$x_min_changed = FALSE
      pp_psi_change$x_max_changed = FALSE
      pp_psi_change$y_min_changed = FALSE
      pp_psi_change$y_max_changed = FALSE

      filtered_pp_psi(
        list(
          prior = pp_df$prior %>%
            filter(
              Scenario %in% input$pp_psi_scenarios
            ),
          posterior = pp_df$posterior %>%
            filter(
              Scenario %in% input$pp_psi_scenarios
            ),
          PPVR = pp_df$PPVR %>%
            filter(
              Scenario %in% input$pp_psi_scenarios
            ),
          PPMR = pp_df$PPMR %>%
            filter(
              Scenario %in% input$pp_psi_scenarios
            )
        )
      )

      title_x_pp_psi(input$pp_psi_title_x)
      title_y_pp_psi(input$pp_psi_title_y)
      prior_color_pp_psi(input$pp_psi_prior_color)
      posterior_color_pp_psi(input$pp_psi_posterior_color)
      text_size_pp_psi(input$pp_psi_text_size)
      x_lim_min_pp_psi(input$pp_psi_x_min)
      x_lim_max_pp_psi(input$pp_psi_x_max)
      y_lim_min_pp_psi(input$pp_psi_y_min)
      y_lim_max_pp_psi(input$pp_psi_y_max)
    }
  }, ignoreInit = TRUE)

  output$priors_posteriors_psi <- renderPlotly({
    req(filtered_pp_psi())
    if (identical(filtered_pp_psi(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_pp_psi()

    palette <- .resolve_palette(
      c(prior_color_pp_psi(), posterior_color_pp_psi()), 
      2
    )

    scenarios <- unique(c(df_lists$prior$Scenario, df_lists$posterior$Scenario))

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    prior <- df_lists$prior %>%
      select(Scenario, psi01, psi02)

    posterior <- df_lists$posterior %>%
      select(Scenario, psi01, psi02)

    x_lim_min <- .get_value_or_default(
      x_lim_min_pp_psi, min(prior$psi01, posterior$psi01, na.rm = TRUE)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_pp_psi, max(prior$psi01, posterior$psi01, na.rm = TRUE)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_pp_psi, 
      .round_to_nearest(min(prior$psi02, posterior$psi02, na.rm = TRUE), 
      FALSE, 1.1)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_pp_psi, 
      .round_to_nearest(max(prior$psi02, posterior$psi02, na.rm = TRUE), 
      TRUE, 1.1)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(
      title_x_pp_psi, "Initial biomass depletion ratio (psi)"
    )

    title_y <- .get_value_or_default(title_y_pp_psi, "Density")

    pos <- .auto_text_position(
      data_list = list(prior, posterior), 
      col_x = "psi01", 
      col_y = "psi02",
      xlim = x_lim,
      ylim = y_lim, 
      margin = 0.25
    )

    PPMR <- df_lists$PPMR %>%
      select(Scenario, psi) %>%
      mutate(x = pos$x, y = pos$y)

    PPVR <- df_lists$PPVR %>%
      select(Scenario, psi) %>%
      mutate(x = pos$x, y = pos$y)

    plots <- map(scenarios, function(s) {
      prior <- prior %>%
        filter(Scenario == s)

      posterior <- posterior %>%
        filter(Scenario == s)

      PPMR <- PPMR %>%
        filter(Scenario == s)

      PPVR <- PPVR %>%
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
        add_trace(
          data = prior,
          x = ~psi01,
          y = ~psi02,
          fillcolor = alpha(palette[1], 0.5),
          fill = "tozeroy",
          type = "scatter",
          mode = "lines",
          line = list(
            width = 0.5,
            color = "black"
          ),
          hoverinfo = "text",
          text = ~paste0(
            "Prior<br>psi01: ", .international_system_prefixes(psi01), 
            "<br>psi02: ", .international_system_prefixes(psi02)
          )
        ) %>%
        add_trace(
          data = posterior,
          x = ~psi01,
          y = ~psi02,
          fillcolor = alpha(palette[2], 0.5),
          fill = "tozeroy",
          type = "scatter",
          mode = "lines",
          line = list(
            width = 0.5,
            color = "black"
          ),
          hoverinfo = "text",
          text = ~paste0(
            "Posterior<br>psi01: ", .international_system_prefixes(psi01), 
            "<br>psi02: ", .international_system_prefixes(psi02)
          )
        ) %>%
        add_text(
          data = PPMR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPMR = ", psi),
          textfont = list(size = text_size_pp_psi()),
          textposition = "top left"
        ) %>%
        add_text(
          data = PPVR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPVR = ", psi),
          textfont = list(size = text_size_pp_psi()),
          textposition = "bottom left"
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
            showticklabels = FALSE,
            title = "",
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
            xshift = -10,
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