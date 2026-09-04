#' @keywords internal
.priors_posteriors_r_server <- function(
  input, output, session, pp_df, use_si_suffix
) {
  filtered_pp_r <- reactiveVal(pp_df)

  title_x_pp_r <- reactiveVal(NULL)

  title_y_pp_r <- reactiveVal(NULL)

  prior_color_pp_r <- reactiveVal(NULL)
  
  posterior_color_pp_r <- reactiveVal(NULL)

  text_size_pp_r <- reactiveVal(16)

  x_lim_min_pp_r <- reactiveVal(NULL)

  x_lim_max_pp_r <- reactiveVal(NULL)

  position_pp_r <- reactiveVal("top-left")

  si_suffix_pp_r <- reactiveVal(use_si_suffix)

  pp_r_change <- reactiveValues(
    scenarios_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    prior_color_changed = FALSE,
    posterior_color_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    position_changed = FALSE,
    si_suffix_changed = FALSE
  )

  pp_r_values <- reactiveValues(
    scenarios_current = unique(c(pp_df$prior$Scenario, 
      pp_df$posterior$Scenario)),
    title_x_current = NA,
    title_y_current = NA,
    prior_color_current = "#1B4F8A",
    posterior_color_current = "#2A9D5C",
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    position_current = "top-left",
    si_suffix_current = use_si_suffix
  )

  observeEvent(input$pp_r_scenarios, {
    if (!setequal(input$pp_r_scenarios, pp_r_values$scenarios_current)) {
      pp_r_change$scenarios_changed = TRUE
    }
    else {
      pp_r_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_title_x, {
    if (!identical(input$pp_r_title_x, pp_r_values$title_x_current)) {
      pp_r_change$title_x_changed = TRUE
    }
    else {
      pp_r_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_title_y, {
    if (!identical(input$pp_r_title_y, pp_r_values$title_y_current)) {
      pp_r_change$title_y_changed = TRUE
    }
    else {
      pp_r_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_prior_color, {
    if (!identical(input$pp_r_prior_color, pp_r_values$prior_color_current)) {
      pp_r_change$prior_color_changed = TRUE
    }
    else {
      pp_r_change$prior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_posterior_color, {
    if (!identical(input$pp_r_posterior_color, 
      pp_r_values$posterior_color_current)) {
      pp_r_change$posterior_color_changed = TRUE
    }
    else {
      pp_r_change$posterior_color_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_text_size, {
    if (!identical(input$pp_r_text_size, pp_r_values$text_size_current)) {
      pp_r_change$text_size_changed = TRUE
    }
    else {
      pp_r_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_x_min, {
    if (!identical(input$pp_r_x_min, pp_r_values$x_min_current)) {
      pp_r_change$x_min_changed = TRUE
    }
    else {
      pp_r_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_x_max, {
    if (!identical(input$pp_r_x_max, pp_r_values$x_max_current)) {
      pp_r_change$x_max_changed = TRUE
    }
    else {
      pp_r_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_position, {
    if (!identical(input$pp_r_position, pp_r_values$position_current)) {
      pp_r_change$position_changed = TRUE
    }
    else {
      pp_r_change$position_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$pp_r_si_suffix, {
    if (!identical(input$pp_r_si_suffix, pp_r_values$si_suffix_current)) {
      pp_r_change$si_suffix_changed = TRUE
    }
    else {
      pp_r_change$si_suffix_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_pp_r <- reactive({
    req(input$navmenu == "tab_priors_posteriors" && 
      input$priors_posteriors_tabs == "tab_pp_r")
    vec <- unlist(reactiveValuesToList(pp_r_change))

    empty_condition <- .is_empty(input$pp_r_scenarios)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_pp_r(), {
    if (status_sliders_pp_r()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  })

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (
      input$navmenu == "tab_priors_posteriors" && 
      input$priors_posteriors_tabs == "tab_pp_r"
    ) {      
      x_min <- input$pp_r_x_min
      x_max <- input$pp_r_x_max

      if (!is.na(x_min) && !is.na(x_max) && x_min > x_max) {
        tmp_x <- x_min
        x_min <- x_max
        x_max <- tmp_x
        updateSelectInput(
          session, inputId = "pp_r_x_min", selected = x_min
        )
        updateSelectInput(
          session, inputId = "pp_r_x_max", selected = x_max
        )
        showNotification(
          ui = "First x value shouldn't be higher than the second x value",
          type = "warning", duration = 10
        )
      }

      pp_r_values$scenarios_current = input$pp_r_scenarios
      pp_r_values$indices_current = input$pp_r_indices
      pp_r_values$title_x_current = input$pp_r_title_x
      pp_r_values$title_y_current = input$pp_r_title_y
      pp_r_values$prior_color_current = input$pp_r_prior_color
      pp_r_values$posterior_color_current = input$pp_r_posterior_color
      pp_r_values$text_size = input$pp_r_text_size
      pp_r_values$x_min_current = input$pp_r_x_min
      pp_r_values$x_max_current = input$pp_r_x_max
      pp_r_values$position_current = input$pp_r_position
      pp_r_values$si_suffix_current = input$pp_r_si_suffix

      pp_r_change$scenarios_changed = FALSE
      pp_r_change$indices_changed = FALSE
      pp_r_change$title_x_changed = FALSE
      pp_r_change$title_y_changed = FALSE
      pp_r_change$color_changed = FALSE
      pp_r_change$text_size_changed = FALSE
      pp_r_change$x_min_changed = FALSE
      pp_r_change$x_max_changed = FALSE
      pp_r_change$position_changed = FALSE
      pp_r_change$si_suffix_changed = FALSE

      filtered_pp_r(
        list(
          prior = pp_df$prior %>%
            filter(
              Scenario %in% input$pp_r_scenarios
            ),
          posterior = pp_df$posterior %>%
            filter(
              Scenario %in% input$pp_r_scenarios
            ),
          PPVR = pp_df$PPVR %>%
            filter(
              Scenario %in% input$pp_r_scenarios
            ),
          PPMR = pp_df$PPMR %>%
            filter(
              Scenario %in% input$pp_r_scenarios
            )
        )
      )

      title_x_pp_r(input$pp_r_title_x)
      title_y_pp_r(input$pp_r_title_y)
      prior_color_pp_r(input$pp_r_prior_color)
      posterior_color_pp_r(input$pp_r_posterior_color)
      text_size_pp_r(input$pp_r_text_size)
      x_lim_min_pp_r(input$pp_r_x_min)
      x_lim_max_pp_r(input$pp_r_x_max)
      position_pp_r(input$pp_r_position)
      si_suffix_pp_r(input$pp_r_si_suffix)
    }
  })

  output$priors_posteriors_r <- renderPlotly({
    req(filtered_pp_r())
    if (identical(filtered_pp_r(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_pp_r()

    palette <- .resolve_palette(
      c(prior_color_pp_r(), posterior_color_pp_r()), 
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

    prior_all <- df_lists$prior %>%
      select(Scenario, r01, r02) %>%
      filter(r02 > 0.005e-9)

    posterior_all <- df_lists$posterior %>%
      select(Scenario, r01, r02) %>%
      filter(r02 > 0.005e-9)

    x_lim_min <- .get_value_or_default(
      x_lim_min_pp_r, min(prior_all$r01, posterior_all$r01, na.rm = TRUE)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_pp_r, max(prior_all$r01, posterior_all$r01, na.rm = TRUE)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .round_to_nearest(
      min(prior_all$r02, posterior_all$r02, na.rm = TRUE), FALSE, 1.1
    )

    y_lim_max <- .round_to_nearest(
      max(prior_all$r02, posterior_all$r02, na.rm = TRUE), TRUE, 1.1
    )
    y_lim <- c(y_lim_min, y_lim_max)

    title_x <- .get_value_or_default(title_x_pp_r, "Intrinsic growth rate (r)")

    title_y <- .get_value_or_default(title_y_pp_r, "Density")

    df_text_all <- df_lists$PPMR %>%
      select(Scenario, ppmr_value = r) %>%
      full_join(
        df_lists$PPVR %>% select(Scenario, ppvr_value = r),
        by = "Scenario"
      )
    
    prior_split <- split(prior_all, prior_all$Scenario)
    posterior_split <- split(posterior_all, posterior_all$Scenario)
    df_text_split <- split(df_text_all, df_text_all$Scenario)

    plots <- map(scenarios, function(s) {
      prior <- prior_split[[s]]
      posterior <- posterior_split[[s]]
      df_text <- df_text_split[[s]]
        
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
      position <- position_pp_r()

      table <- .build_metric_table(
        df_text, text_size_pp_r(), 
        str_split_i(position, "-", 2),
        str_split_i(position, "-", 1), 
        c("ppmr_value", "ppvr_value"), 
        c("PPMR", "PPVR"), decimals = 3
      )

      shapes <- append(shapes, table$shapes)

      annotations <- append(annotations, table$annotations)

      plot_ly() %>%
        add_trace(
          data = prior,
          x = ~r01,
          y = ~r02,
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
            "Prior<br>r: ", 
            .international_system_prefixes(r01, si_suffix_pp_r())
          )
        ) %>%
        add_trace(
          data = posterior,
          x = ~r01,
          y = ~r02,
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
            "Posterior<br>r: ", 
            .international_system_prefixes(r01, si_suffix_pp_r())
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
    # toc()
    results
  })
}