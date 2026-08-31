#' @keywords internal
.cpue_res_server <- function(input, output, session, res_df) {
  filtered_cpue_res <- reactiveVal(res_df)

  title_x_cpue_res <- reactiveVal(NULL)

  title_y_cpue_res <- reactiveVal(NULL)

  palette_cpue_res <- reactiveVal(NULL)

  text_size_cpue_res <- reactiveVal(16)

  x_lim_min_cpue_res <- reactiveVal(NULL)

  x_lim_max_cpue_res <- reactiveVal(NULL)

  y_lim_min_cpue_res <- reactiveVal(NULL)

  y_lim_max_cpue_res <- reactiveVal(NULL)

  position_cpue_res <- reactiveVal("top-left")

  cpue_res_change <- reactiveValues(
    scenarios_changed = FALSE,
    indices_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    color_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE,
    position_changed = FALSE
  )

  cpue_res_values <- reactiveValues(
    scenarios_current = unique(res_df$cpue_residuals$Scenario),
    indices_current = unique(res_df$cpue_residuals$Index),
    title_x_current = NA,
    title_y_current = NA,
    color_current = NULL,
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA,
    position_current = "top-left"
  )

  observeEvent(input$cpue_res_scenarios, {
    if (!setequal(input$cpue_res_scenarios, cpue_res_values$scenarios_current)){
      cpue_res_change$scenarios_changed = TRUE
    }
    else {
      cpue_res_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_indices, {
    if (!setequal(input$cpue_res_indices, cpue_res_values$indices_current)) {
      cpue_res_change$indices_changed = TRUE
    }
    else {
      cpue_res_change$indices_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_title_x, {
    if (!identical(input$cpue_res_title_x, cpue_res_values$title_x_current)) {
      cpue_res_change$title_x_changed = TRUE
    }
    else {
      cpue_res_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_title_y, {
    if (!identical(input$cpue_res_title_y, cpue_res_values$title_y_current)) {
      cpue_res_change$title_y_changed = TRUE
    }
    else {
      cpue_res_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_text_size, {
    if(!identical(input$cpue_res_text_size, cpue_res_values$text_size_current)){
      cpue_res_change$text_size_changed = TRUE
    }
    else {
      cpue_res_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_x_min, {
    if (!identical(input$cpue_res_x_min, cpue_res_values$x_min_current)) {
      cpue_res_change$x_min_changed = TRUE
    }
    else {
      cpue_res_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_x_max, {
    if (!identical(input$cpue_res_x_max, cpue_res_values$x_max_current)) {
      cpue_res_change$x_max_changed = TRUE
    }
    else {
      cpue_res_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_y_min, {
    if (!identical(input$cpue_res_y_min, cpue_res_values$y_min_current)) {
      cpue_res_change$y_min_changed = TRUE
    }
    else {
      cpue_res_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_y_max, {
    if (!identical(input$cpue_res_y_max, cpue_res_values$y_max_current)) {
      cpue_res_change$y_max_changed = TRUE
    }
    else {
      cpue_res_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cpue_res_position, {
    if (!identical(input$cpue_res_position, cpue_res_values$position_current)) {
      cpue_res_change$position_changed = TRUE
    }
    else {
      cpue_res_change$position_changed = FALSE
    }
  }, ignoreInit = TRUE)

  current_colors_cpue_res <- reactive({
    n <- length(unique(res_df$cpue_residuals$Index))
    req(n > 0)
    var <- c()

    sapply(
      seq_len(n),
      function(i) {
        var <- append(var, input[[paste0("cpue_res_color_", i)]])
      }
    )
  })

  observeEvent(current_colors_cpue_res(), {
    if (!setequal(current_colors_cpue_res(), cpue_res_values$color_current)) {
      cpue_res_change$color_changed = TRUE
    }
    else {
      cpue_res_change$color_changed = FALSE
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  output$cpue_res_color_inputs <- renderUI({
    n <- length(unique(res_df$cpue_residuals$Index))

    default_palette <- .resolve_palette(NULL, n)

    color_inputs <- lapply(seq_len(n), function(i) {
      colourInput(
        inputId = paste0("cpue_res_color_", i),
        label = paste("Color", i, ": "),
        value = default_palette[i]
      )
    })

    tagList(color_inputs)
  })

  status_sliders_cpue_res <- reactive({
    req(input$navmenu == "tab_cpue_residuals")
    vec <- unlist(reactiveValuesToList(cpue_res_change))

    empty_condition <- .is_empty(input$cpue_res_scenarios)|| 
      .is_empty(input$cpue_res_indices)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_cpue_res(), {
    if (status_sliders_cpue_res()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_cpue_residuals") {

      selected_colors <- current_colors_cpue_res()

      cpue_res_values$scenarios_current = input$cpue_res_scenarios
      cpue_res_values$indices_current = input$cpue_res_indices
      cpue_res_values$title_x_current = input$cpue_res_title_x
      cpue_res_values$title_y_current = input$cpue_res_title_y
      cpue_res_values$color_current = selected_colors
      cpue_res_values$text_size = input$cpue_res_text_size
      cpue_res_values$x_min_current = input$cpue_res_x_min
      cpue_res_values$x_max_current = input$cpue_res_x_max
      cpue_res_values$y_min_current = input$cpue_res_y_min
      cpue_res_values$y_max_current = input$cpue_res_y_max
      cpue_res_values$position_current = input$cpue_res_position

      cpue_res_change$scenarios_changed = FALSE
      cpue_res_change$indices_changed = FALSE
      cpue_res_change$title_x_changed = FALSE
      cpue_res_change$title_y_changed = FALSE
      cpue_res_change$color_changed = FALSE
      cpue_res_change$text_size_changed = FALSE
      cpue_res_change$x_min_changed = FALSE
      cpue_res_change$x_max_changed = FALSE
      cpue_res_change$y_min_changed = FALSE
      cpue_res_change$y_max_changed = FALSE
      cpue_res_change$position_changed = FALSE

      filtered_cpue_res(
        list(
          cpue_residuals = res_df$cpue_residuals %>% filter(
            Scenario %in% input$cpue_res_scenarios,
            Index %in% input$cpue_res_indices
          ) %>% droplevels(),
          SE3 = res_df$SE3 %>% filter(
            Scenario %in% input$cpue_res_scenarios,
            Index %in% input$cpue_res_indices
          ) %>% droplevels(),
          RMSE_data = res_df$RMSE_data %>% filter(
            Scenario %in% input$cpue_res_scenarios
          )
        )
      )
      title_x_cpue_res(input$cpue_res_title_x)
      title_y_cpue_res(input$cpue_res_title_y)
      palette_cpue_res(selected_colors)
      text_size_cpue_res(input$cpue_res_text_size)
      x_lim_min_cpue_res(input$cpue_res_x_min)
      x_lim_max_cpue_res(input$cpue_res_x_max)
      y_lim_min_cpue_res(input$cpue_res_y_min)
      y_lim_max_cpue_res(input$cpue_res_y_max)
      position_cpue_res(input$cpue_res_position)
    }
  }, ignoreInit = TRUE)

  output$cpue_residuals <- renderPlotly({
    req(filtered_cpue_res())
    if (identical(filtered_cpue_res(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_cpue_res()

    x_lim_min <- .get_value_or_default(
      x_lim_min_cpue_res, min(df_lists$cpue_residuals$Year, na.rm = TRUE)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_cpue_res, max(df_lists$cpue_residuals$Year, na.rm = TRUE)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_cpue_res, 
      .round_to_nearest(min(df_lists$cpue_residuals$Res, na.rm = TRUE), FALSE)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_cpue_res, 
      .round_to_nearest(max(df_lists$cpue_residuals$Res, na.rm = TRUE), TRUE)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df_lists$cpue_residuals$Scenario)

    n_indices <- length(unique(df_lists$cpue_residuals$Index))

    n_scenarios <- length(scenarios)
    
    title_x <- .get_value_or_default(title_x_cpue_res, "Year")

    title_y <- .get_value_or_default(title_y_cpue_res, "Residuals")

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }
    palette <- .resolve_palette(palette_cpue_res(), n_indices)

    plots <- map(scenarios, function(s) {
      cpue_residuals <- df_lists$cpue_residuals %>%
        filter(Scenario == s)

      RMSE_data <- df_lists$RMSE_data %>%
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
      position <- position_cpue_res()

      table <- .build_metric_table(
        RMSE_data, text_size_cpue_res(), 
        str_split_i(position, "-", 2),
        str_split_i(position, "-", 1), 
        "Value", "RMSE", "%"
      )

      shapes <- append(shapes, table$shapes)

      annotations <- append(annotations, table$annotations)
      
      plot_ly(
        data = cpue_residuals,
        x = ~Year
      ) %>%
        add_segments(
          xend = ~Year,
          y = ~Ref,
          yend = ~Res,
          color = ~Index,
          colors = palette,
          hoverinfo = "none"
        ) %>%
        add_markers(
          y = ~Res,
          color = ~Index,
          colors = palette,
          marker = list(
            size = 8,
            line = list(width = 0)
          ),
          hoverinfo = "text+x",
          text = ~paste0(
            "Index: ", Index, 
            "<br>Residuals: ", .international_system_prefixes(Res, 2)
          )
        ) %>%
        add_lines(
          y = ~fit,
          line = list(width = 2, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0(
            "Loess: ", .international_system_prefixes(fit, 2)
          )
        ) %>%
        add_ribbons(
          ymin = ~lower,
          ymax = ~upper,
          fillcolor = "gray",
          opacity = 0.3,
          line = list(width = 0),
          hoverinfo = "text+x",
          text = ~paste0(
            "CI(95): (", .international_system_prefixes(lower, 2), ") - (", 
            .international_system_prefixes(upper, 2), ")"
          ) 
        ) %>%
          add_segments(
            x = x_lim[1],
            xend = x_lim[2],
            y = 0, 
            yend = 0,
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
    # toc()
    results
  })
}