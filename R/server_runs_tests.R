#' @keywords internal
.runs_tests_server <- function(input, output, session, res_df) {
  filtered_runs_tests <- reactiveVal(res_df)

  title_x_runs_tests <- reactiveVal(NULL)

  title_y_runs_tests <- reactiveVal(NULL)

  text_size_runs_tests <- reactiveVal(16)

  x_lim_min_runs_tests <- reactiveVal(NULL)

  x_lim_max_runs_tests <- reactiveVal(NULL)

  y_lim_min_runs_tests <- reactiveVal(NULL)

  y_lim_max_runs_tests <- reactiveVal(NULL)

  position_runs_tests <- reactiveVal("top-left")

  runs_tests_change <- reactiveValues(
    scenarios_changed = FALSE,
    indices_changed = FALSE,
    title_x_changed = FALSE,
    title_y_changed = FALSE,
    text_size_changed = FALSE,
    x_min_changed = FALSE,
    x_max_changed = FALSE,
    y_min_changed = FALSE,
    y_max_changed = FALSE,
    position_changed = FALSE
  )

  runs_tests_values <- reactiveValues(
    scenarios_current = unique(res_df$cpue_residuals$Scenario),
    indices_current = unique(res_df$cpue_residuals$Index),
    title_x_current = NA,
    title_y_current = NA,
    text_size_current = 16,
    x_min_current = NA,
    x_max_current = NA,
    y_min_current = NA,
    y_max_current = NA,
    position_current = "top-left"
  )

  observeEvent(input$runs_tests_scenarios, {
    if (!setequal(input$runs_tests_scenarios, 
      runs_tests_values$scenarios_current)) {
      runs_tests_change$scenarios_changed = TRUE
    }
    else {
      runs_tests_change$scenarios_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_indices, {
    if(!setequal(input$runs_tests_indices, runs_tests_values$indices_current)) {
      runs_tests_change$indices_changed = TRUE
    }
    else {
      runs_tests_change$indices_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_title_x, {
    if(!identical(input$runs_tests_title_x, runs_tests_values$title_x_current)){
      runs_tests_change$title_x_changed = TRUE
    }
    else {
      runs_tests_change$title_x_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_title_y, {
    if(!identical(input$runs_tests_title_y, runs_tests_values$title_y_current)){
      runs_tests_change$title_y_changed = TRUE
    }
    else {
      runs_tests_change$title_y_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_text_size, {
    if (!identical(input$runs_tests_text_size, runs_tests_values$text_size_current)) {
      runs_tests_change$text_size_changed = TRUE
    }
    else {
      runs_tests_change$text_size_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_x_min, {
    if (!identical(input$runs_tests_x_min, runs_tests_values$x_min_current)) {
      runs_tests_change$x_min_changed = TRUE
    }
    else {
      runs_tests_change$x_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_x_max, {
    if (!identical(input$runs_tests_x_max, runs_tests_values$x_max_current)) {
      runs_tests_change$x_max_changed = TRUE
    }
    else {
      runs_tests_change$x_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_y_min, {
    if (!identical(input$runs_tests_y_min, runs_tests_values$y_min_current)) {
      runs_tests_change$y_min_changed = TRUE
    }
    else {
      runs_tests_change$y_min_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_y_max, {
    if (!identical(input$runs_tests_y_max, runs_tests_values$y_max_current)) {
      runs_tests_change$y_max_changed = TRUE
    }
    else {
      runs_tests_change$y_max_changed = FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input$runs_tests_position, {
    if (!identical(input$runs_tests_position, 
      runs_tests_values$position_current)) {
      runs_tests_change$position_changed = TRUE
    }
    else {
      runs_tests_change$position_changed = FALSE
    }
  }, ignoreInit = TRUE)

  status_sliders_runs_tests <- reactive({
    req(input$navmenu == "tab_runs_tests")
    vec <- unlist(reactiveValuesToList(runs_tests_change))

    empty_condition <- .is_empty(input$runs_tests_scenarios)|| 
      .is_empty(input$runs_tests_indices)
    
    enable <- any(vec) && !empty_condition

    return(enable)
  })

  observeEvent(status_sliders_runs_tests(), {
    if (status_sliders_runs_tests()) {
      enable("confirm_button")
    } else {
      disable("confirm_button")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_button, {
    updateControlbar(id = "controlbar", session = session)

    if (input$navmenu == "tab_runs_tests") {
      runs_tests_values$scenarios_current = input$runs_tests_scenarios
      runs_tests_values$indices_current = input$runs_tests_indices
      runs_tests_values$title_x_current = input$runs_tests_title_x
      runs_tests_values$title_y_current = input$runs_tests_title_y
      runs_tests_values$text_size_current = input$runs_tests_text_size
      runs_tests_values$x_min_current = input$runs_tests_x_min
      runs_tests_values$x_max_current = input$runs_tests_x_max
      runs_tests_values$y_min_current = input$runs_tests_y_min
      runs_tests_values$y_max_current = input$runs_tests_y_max
      runs_tests_values$position_current = input$runs_tests_position

      runs_tests_change$scenarios_changed = FALSE
      runs_tests_change$indices_changed = FALSE
      runs_tests_change$title_x_changed = FALSE
      runs_tests_change$title_y_changed = FALSE
      runs_tests_change$text_size_changed = FALSE
      runs_tests_change$x_min_changed = FALSE
      runs_tests_change$x_max_changed = FALSE
      runs_tests_change$y_min_changed = FALSE
      runs_tests_change$y_max_changed = FALSE
      runs_tests_change$position_changed = FALSE

      filtered_runs_tests(
        list(
          cpue_residuals = res_df$cpue_residuals %>% filter(
            Scenario %in% input$runs_tests_scenarios,
            Index %in% input$runs_tests_indices
          ) %>% droplevels(),
          SE3 = res_df$SE3 %>% filter(
            Scenario %in% input$runs_tests_scenarios,
            Index %in% input$runs_tests_indices
          ) %>% droplevels(),
          RMSE_data = res_df$RMSE_data %>% filter(
            Scenario %in% input$runs_tests_scenarios
          )
        )
      )
      title_x_runs_tests(input$runs_tests_title_x)
      title_y_runs_tests(input$runs_tests_title_y)
      text_size_runs_tests(input$runs_tests_text_size)
      x_lim_min_runs_tests(input$runs_tests_x_min)
      x_lim_max_runs_tests(input$runs_tests_x_max)
      y_lim_min_runs_tests(input$runs_tests_y_min)
      y_lim_max_runs_tests(input$runs_tests_y_max)
      position_runs_tests(input$runs_tests_position)
    }
  }, ignoreInit = TRUE)

  output$runs_tests <- renderPlotly({
    req(filtered_runs_tests())
    if (identical(filtered_runs_tests(), list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- filtered_runs_tests()
    
    x_lim_min <- .get_value_or_default(
      x_lim_min_runs_tests, min(df_lists$SE3$ymin)
    )

    x_lim_max <- .get_value_or_default(
      x_lim_max_runs_tests, max(df_lists$SE3$ymax)
    )
    x_lim <- c(x_lim_min, x_lim_max)

    y_lim_min <- .get_value_or_default(
      y_lim_min_runs_tests, 
      .round_to_nearest(min(df_lists$SE3$lcl, na.rm = TRUE), FALSE, 2.5)
    )

    y_lim_max <- .get_value_or_default(
      y_lim_max_runs_tests, 
      .round_to_nearest(max(df_lists$SE3$ucl, na.rm = TRUE), TRUE, 2.5)
    )
    y_lim <- c(y_lim_min, y_lim_max)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df_lists$cpue_residuals$Scenario)
    indices <- levels(df_lists$cpue_residuals$Index)

    n_scenarios <- length(scenarios)
    n_indices <- length(indices)

    title_x <- .get_value_or_default(title_x_runs_tests, "Year")

    title_y <- .get_value_or_default(title_y_runs_tests, "Residuals")

    plots <- map(scenarios, function(s) {
      map(indices, function(i) {
        SE3 <- df_lists$SE3 %>%
          filter(Scenario == s, Index == i)

        cpue_residuals <- df_lists$cpue_residuals %>%
          filter(Scenario == s, Index == i)
        
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
              line = list(width = 0),
              y0 = SE3$lcl,
              y1 = SE3$ucl,
              x0 = SE3$ymin,
              x1 = SE3$ymax,
              fillcolor = SE3$class,
              opacity = 0.2,
              layer = "below"
            )
          )
        )

        if (s == scenarios[1]) {
          shapes <- append(
            shapes,
            list(
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
                text = i,
                showarrow = FALSE,
                font = list(
                  size = 20,
                  color = "white"
                )
              )
            )
          )
        }

        if (i == indices[n_indices]) {
          shapes <- append(
            shapes,
            list(
              list(
                type = "rect",
                xref = "paper",
                yref = "paper",
                xanchor = 1,
                x0 = 0,
                x1 = 28,
                y0 = 0, 
                y1 = 1,
                xsizemode = "pixel",
                line = list(width = 1),
                fillcolor = "black"
              )
            )
          )
          annotations <- append(
            annotations,
            list(
              list(
                x = 1,
                y = 0.5,
                textangle = 90,
                xanchor = "center",
                yanchor = "middle",
                xshift = 15,
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
        }

        p <- plot_ly() %>%
          add_segments(
            data = cpue_residuals,
            x = ~Year,
            xend = ~Year,
            y = ~Ref, 
            yend = ~Res,
            line = list(color = "black"),
            hoverinfo = "text+x",
            text = ~paste0(
              "CI(95): ", .international_system_prefixes(lcl, 2), " - ",
              .international_system_prefixes(ucl, 2)  
            ) 
          ) %>%
          add_markers(
            data = cpue_residuals %>% filter(class == "white"),
            x = ~Year,
            y = ~Res,
            marker = list(
              color = "white",
              size = 8,
              line = list(
                color = "black",
                width = 2
              )
            ),
            hoverinfo = "text+x",
            text = ~paste0("Residue: ", .international_system_prefixes(Res, 2))
          ) %>%
          add_markers(
            data = cpue_residuals %>% filter(class == "red"),
            x = ~Year,
            y = ~Res,
            marker = list(
              color = "red",
              size = 8,
              line = list(
                color = "black",
                width = 2
              )
            ),
            hoverinfo = "text+x",
            text = ~paste0("Residue: ", .international_system_prefixes(Res, 2))
          )
        
        if (nrow(SE3) != 0) {
          p <- p %>%
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
            )
          position <- position_runs_tests()
          
          table <- .build_metric_table(
            SE3, text_size_runs_tests(), 
            str_split_i(position, "-", 2),
            str_split_i(position, "-", 1), 
            "pvalue", "p-value", decimals = 3
          )
          shapes <- append(shapes, table$shapes)

          annotations <- append(annotations, table$annotations)
        }
        
        p <- p %>%
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
        p
        
      })
    }) %>% flatten()
    

    results <- subplot(
      plots,
      nrows = length(scenarios),
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
    # toc()
    results
  })
}