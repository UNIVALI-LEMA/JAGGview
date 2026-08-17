#' @keywords internal
.priors_posteriors_K_server <- function(input, output, session, pp_df) {
  filtered_pp_K <- reactiveVal(pp_df)

  title_x_pp_K <- reactiveVal(NULL)

  title_y_pp_K <- reactiveVal(NULL)

  prior_color_pp_K <- reactiveVal(NULL)
  
  posterior_color_pp_K <- reactiveVal(NULL)

  text_size_pp_K <- reactiveVal(16)

  x_lim_min_pp_K <- reactiveVal(NULL)

  x_lim_max_pp_K <- reactiveVal(NULL)

  y_lim_min_pp_K <- reactiveVal(NULL)

  y_lim_max_pp_K <- reactiveVal(NULL)

  pp_K_change <- reactiveValues(
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

  pp_K_values <- reactiveValues(
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

  observeEvent(input$pp_K_scenarios, {
    if (!setequal(input$pp_K_scenarios, pp_K_values$scenarios_current)) {
      pp_K_change$scenarios_changed = TRUE
    }
    else {
      pp_K_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_title_x, {
    if (!identical(input$pp_K_title_x, pp_K_values$title_x_current)) {
      pp_K_change$title_x_changed = TRUE
    }
    else {
      pp_K_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_title_y, {
    if (!identical(input$pp_K_title_y, pp_K_values$title_y_current)) {
      pp_K_change$title_y_changed = TRUE
    }
    else {
      pp_K_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_prior_color, {
    if (!identical(input$pp_K_prior_color, pp_K_values$prior_color_current)) {
      pp_K_change$prior_color_changed = TRUE
    }
    else {
      pp_K_change$prior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_posterior_color, {
    if (!identical(input$pp_K_posterior_color, 
      pp_K_values$posterior_color_current)) {
      pp_K_change$posterior_color_changed = TRUE
    }
    else {
      pp_K_change$posterior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_text_size, {
    if (!identical(input$pp_K_text_size, pp_K_values$text_size_current)) {
      pp_K_change$text_size_changed = TRUE
    }
    else {
      pp_K_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_x_min, {
    if (!identical(input$pp_K_x_min, pp_K_values$x_min_current)) {
      pp_K_change$x_min_changed = TRUE
    }
    else {
      pp_K_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_x_max, {
    if (!identical(input$pp_K_x_max, pp_K_values$x_max_current)) {
      pp_K_change$x_max_changed = TRUE
    }
    else {
      pp_K_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_y_min, {
    if (!identical(input$pp_K_y_min, pp_K_values$y_min_current)) {
      pp_K_change$y_min_changed = TRUE
    }
    else {
      pp_K_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_K_y_max, {
    if (!identical(input$pp_K_y_max, pp_K_values$y_max_current)) {
      pp_K_change$y_max_changed = TRUE
    }
    else {
      pp_K_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_pp_K <- reactive({
    vec <- unlist(reactiveValuesToList(pp_K_change))

    empty_condition <- .is_empty(input$pp_K_scenarios)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_pp_K(), {
    if (status_sliders_pp_K()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_priors_posteriors" && 
      input$priors_posteriors_tabs == "tab_pp_K") {
      pp_K_values$scenarios_current = input$pp_K_scenarios
      pp_K_values$indices_current = input$pp_K_indices
      pp_K_values$title_x_current = input$pp_K_title_x
      pp_K_values$title_y_current = input$pp_K_title_y
      pp_K_values$prior_color_current = input$pp_K_prior_color
      pp_K_values$posterior_color_current = input$pp_K_posterior_color
      pp_K_values$text_size = input$pp_K_text_size
      pp_K_values$x_min_current = input$pp_K_x_min
      pp_K_values$x_max_current = input$pp_K_x_max
      pp_K_values$y_min_current = input$pp_K_y_min
      pp_K_values$y_max_current = input$pp_K_y_max

      pp_K_change$scenarios_changed = FALSE
      pp_K_change$indices_changed = FALSE
      pp_K_change$title_x_changed = FALSE
      pp_K_change$title_y_changed = FALSE
      pp_K_change$color_changed = FALSE
      pp_K_change$text_size_changed = FALSE
      pp_K_change$x_min_changed = FALSE
      pp_K_change$x_max_changed = FALSE
      pp_K_change$y_min_changed = FALSE
      pp_K_change$y_max_changed = FALSE

      filtered_pp_K(
        list(
          prior = pp_df$prior %>%
            filter(
              Scenario %in% input$pp_K_scenarios
            ),
          posterior = pp_df$posterior %>%
            filter(
              Scenario %in% input$pp_K_scenarios
            ),
          PPVR = pp_df$PPVR %>%
            filter(
              Scenario %in% input$pp_K_scenarios
            ),
          PPMR = pp_df$PPMR %>%
            filter(
              Scenario %in% input$pp_K_scenarios
            )
        )
      )

      title_x_pp_K(input$pp_K_title_x)
      title_y_pp_K(input$pp_K_title_y)
      prior_color_pp_K(input$pp_K_prior_color)
      posterior_color_pp_K(input$pp_K_posterior_color)
      text_size_pp_K(input$pp_K_text_size)
      x_lim_min_pp_K(input$pp_K_x_min)
      x_lim_max_pp_K(input$pp_K_x_max)
      y_lim_min_pp_K(input$pp_K_y_min)
      y_lim_max_pp_K(input$pp_K_y_max)
    }
  }, ignoreInit = TRUE)

  output$priors_posteriors_K <- renderPlotly({
    req(filtered_pp_K())
    if (identical(filtered_pp_K(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_pp_K()

    palette <- .resolve_palette(
      c(prior_color_pp_K(), posterior_color_pp_K()), 
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
      select(Scenario, K01, K02)

    posterior <- df_lists$posterior %>%
      select(Scenario, K01, K02)

    x_lim_min <- .get_value_or_default(
      x_lim_min_pp_K, min(prior$K01, posterior$K01, na.rm = TRUE)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_pp_K, max(prior$K01, posterior$K01, na.rm = TRUE)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_pp_K, 
      .round_to_nearest(min(prior$K02, posterior$K02, na.rm = TRUE), FALSE, 1.1)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_pp_K, 
      .round_to_nearest(max(prior$K02, posterior$K02, na.rm = TRUE), TRUE, 1.1)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_pp_K, "Carrying capacity (K)")

    title_y <- .get_value_or_default(title_y_pp_K, "Density")

    pos <- .auto_text_position(
      data_list = list(prior, posterior), 
      col_x = "K01", 
      col_y = "K02",
      xlim = x_lim,
      ylim = y_lim, 
      margin = 0.2
    )

    PPMR <- df_lists$PPMR %>%
      select(Scenario, K) %>%
      mutate(x = pos$x, y = pos$y)


    PPVR <- df_lists$PPVR %>%
      select(Scenario, K) %>%
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
          x = ~K01,
          y = ~K02,
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
            "Prior<br>K01: ", .international_system_prefixes(K01), 
            "<br>K02: ", .international_system_prefixes(K02)
          )
        ) %>%
        add_trace(
          data = posterior,
          x = ~K01,
          y = ~K02,
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
            "Posterior<br>K01: ", .international_system_prefixes(K01), 
            "<br>K02: ", .international_system_prefixes(K02)
          )
        ) %>%
        add_text(
          data = PPMR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPMR = ", K),
          textfont = list(size = text_size_pp_K()),
          textposition = "top center"
        ) %>%
        add_text(
          data = PPVR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPVR = ", K),
          textfont = list(size = text_size_pp_K()),
          textposition = "bottom center"
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