server <- function(input, output, session) {
  output$cpue_residuals <- renderPlotly({
    if (identical(res_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- res_df

    max_y_val <- .round_to_nearest(max(
      df_lists$cpue_residuals$Res, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(
      df_lists$cpue_residuals$Res, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
  
    max_x_val <- max(df_lists$cpue_residuals$Year, na.rm = TRUE)
    min_x_val <- min(df_lists$cpue_residuals$Year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df_lists$cpue_residuals$Scenario)

    n_indices <- length(unique(df_lists$cpue_residuals$Index))

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, n_indices)

    pos <- .auto_text_position(
      data_list = df_lists$cpue_residuals,
      col_x = "Year",
      col_y = "Res",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.25
    )

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
        add_text(
          inherit = FALSE,
          data = RMSE_data,
          x = pos$x,
          y = pos$y,
          text = ~paste0("RMSE = ", Value, "%"),
          textfont = list(size = 16),
          hoverinfo = "none"
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
    }) %>% flatten()

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
            text = "Year",
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
            text = "Residuals",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$fits <- renderPlotly({
    if (identical(fits_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    palette <- .resolve_palette(NULL, 1)

    df <- fits_df

    max_y_val <- .round_to_nearest(max(df$uci_95, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(df$lci_95, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$Year, na.rm = TRUE)
    min_x_val <- min(df$Year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df$Scenario)
    indices <- levels(df$Index)

    n_scenarios <- length(scenarios)
    n_indices <- length(indices)

    plots <- map(scenarios, function(s) {
      map(indices, function(i) {

        df <- df %>%
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
        
        plot_ly(
            data = df,
            x = ~Year
          ) %>%
          add_ribbons(
            ymin = ~lci_95,
            ymax = ~uci_95,
            fillcolor = palette[1],
            opacity = 0.3,
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "CI(95%): ", 
              .international_system_prefixes(lci_95, 2), "-", 
              .international_system_prefixes(uci_95, 2)
            )
          ) %>%
          add_ribbons(
            ymin = ~lci_80,
            ymax = ~uci_80,
            fillcolor = palette[1],
            opacity = 0.3,
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "CI(80%): ", 
              .international_system_prefixes(lci_80, 2), "-", 
              .international_system_prefixes(uci_80, 2)  
            )
          ) %>%
          add_lines(
            y = ~mu_80,
            type = "scatter",
            mode = "lines",
            line = list(width = 2, color = "black"),
            hoverinfo = "text+x",
            text = ~paste0(
              "Mean: ", .international_system_prefixes(mu_80, 2)
            )
          ) %>%
          add_markers(
            y = ~Mean,
            marker = list(
              color = "white",
              line = list(
                color = "black",
                width = 2
              )
            ),
            error_y = list(
              type = "data",
              array = ~error,
              color = "black"
            ),
            hoverinfo = "text+x",
            text = ~paste0(
              "Point: ", .international_system_prefixes(Mean, 2), 
              "<br>Interval: ", .international_system_prefixes(Li, 2), "-", 
              .international_system_prefixes(Ui, 2)
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
    }) %>% flatten()

    subplot(
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
            text = "Year",
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
            xshift = -20,
            xref = "paper",
            yref = "paper",
            text = "Abundance index",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })
  
  output$hindcast <- renderPlotly({
    if (identical(hind_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- hind_df

    scenarios <- unique(df_lists$data$Scenario)
    indices <- levels(df_lists$data$Index)
    
    n_scenarios <- length(scenarios)
    n_indices <- length(indices)
    
    nrow <- case_when(
      n_scenarios > n_indices ~ n_scenarios,
      n_scenarios < 3          ~ 1,
      n_scenarios < 8          ~ 2,
      TRUE                     ~ 3
    )

    max_y_val <- .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)

    max_x_val <- max(df_lists$data$year)
    min_x_val <- min(df_lists$data$year)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    pos <- .auto_text_position(
      df_lists$data, 
      "year", 
      "hat.uci", 
      xlim = x_lim,
      ylim = y_lim
    )

    min_year_hc <- min(df_lists$hindcast_data_2$year) - 1

    max_year_hc <- max(df_lists$hindcast_data_2$year)
  
    plots <- map(scenarios, function(s) {
      map(indices, function(i) {
        data <- df_lists$data %>%
          filter(Scenario == s, Index == i)

        hc_data_1 <- df_lists$hindcast_data_1 %>%
          filter(Scenario == s, Index == i)

        hc_data_2 <- df_lists$hindcast_data_2 %>%
          filter(Scenario == s, Index == i)

        mase_data <- df_lists$mase_data %>%
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

        p <- plot_ly(colors = c("black", ss3col(8))) %>%
          add_ribbons(
            data = filter(data, retro.peels == 0),
            x = ~year,
            ymin = ~hat.lci,
            ymax = ~hat.uci,
            fillcolor = alpha("gray", alpha = 0.4),
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat CI(95): (", .international_system_prefixes(hat.lci, 2), 
              ") - (", .international_system_prefixes(hat.uci, 2), ")"
            )
          ) %>%
          add_ribbons(
            data = filter(data, retro.peels == 0, 
              year < df_lists$min_year_retro),
            x = ~year,
            ymin = ~hat.lci,
            ymax = ~hat.uci,
            fillcolor = alpha("gray", alpha = 0.9),
            line = list(width = 0),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat CI(95): (", .international_system_prefixes(hat.lci, 2), 
              ") - (", .international_system_prefixes(hat.uci, 2), ")"
            )
          ) %>%
          add_lines(
            data = filter(data, hindcast == FALSE),
            x = ~year,
            y = ~hat,
            color = ~as.factor(retro),
            type = "scatter",
            mode = "lines",
            line = list(width = 2),
            hoverinfo = "text+x",
            text = ~paste0(
              "hat (", retro,"): ", .international_system_prefixes(hat, 2)
            ),
            inherit = FALSE
          ) %>%
          add_lines(
            data = hc_data_2,
            x = ~year,
            y = ~hat,
            line = list(width = 2, color = "black"),
            split = ~retro.peels,
            hoverinfo = "text+x",
            text = ~paste0(
              "hat - hindcast(", retro,"): ", 
              .international_system_prefixes(hat, 2)
            )
          ) %>%
          add_markers(
            data = filter(data, retro.peels == 0, 
              year < df_lists$min_year_retro),
            x = ~year,
            y = ~obs,
            marker = list(
              size = 10,
              color = "white",
              line = list(
                width = 1,
                color = "black"
              )
            ),
            hoverinfo = "text+x",
            text = ~paste0(
              "obs: ", .international_system_prefixes(obs, 2)
            )
          ) %>%
          add_markers(
            data = hc_data_1,
            x = ~year,
            y = ~obs,
            color = ~as.factor(retro),
            split = ~retro,
            marker = list(
              size = 10,
              line = list(
                width = 1,
                color = "black"
              )
            ),
            hoverinfo = "text+x",
            text = ~paste0(
              "hindcast obs (", retro,"): ",
              .international_system_prefixes(obs, 2)
            )
          ) %>%
          add_markers(
            data = hc_data_1,
            x = ~year,
            y = ~hat,
            color = ~as.factor(retro),
            split = ~retro,
            marker = list(
              size = 5,
              line = list(
                width = 1,
                color = "black"
              )
            ),
            hoverinfo = "text+x",
            text = ~paste0(
              "hindcast hat (", retro,"): ", 
              .international_system_prefixes(hat, 2)
            )
          ) 
        if (nrow(mase_data)) {
          p <- p %>%
            add_text(
              data = mase_data,
              x = pos$x,
              y = pos$y,
              hoverinfo = "none",
              textfont = list(size = 16),
              text = ~paste0(
                "MASE = ", .international_system_prefixes(MASE, decimals = 3)
              )
            )
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
            text = "Year",
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
            xshift = -20,
            xref = "paper",
            yref = "paper",
            text = "Index",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$kobe <- renderPlotly({
    if (identical(kobe_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- kobe_df

    max_y <- df_lists$col02$ymax
    y_lim <- c(0, max_y)

    max_x <- df_lists$col02$xmax
    x_lim <- c(0, max_x)

    scenarios <- unique(df_lists$k.out$Scenario)

    n_levels <- length(unique(df_lists$k.out$q))

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- colorRampPalette(c("cornsilk4", "grey", "cornsilk2"))(n_levels)

    plots <- map(scenarios, function(s) {
      line_data <- df_lists$tmp11 %>%
        filter(Scenario == s)

      marker_data <- df_lists$tmp11b %>%
        filter(Scenario == s)

      ci_data <- df_lists$k.out %>%
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
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col01$ymin,
            y1 = df_lists$col01$ymax,
            x0 = df_lists$col01$xmin,
            x1 = df_lists$col01$xmax,
            fillcolor = df_lists$col01$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col02$ymin,
            y1 = df_lists$col02$ymax,
            x0 = df_lists$col02$xmin,
            x1 = df_lists$col02$xmax,
            fillcolor = df_lists$col02$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col03$ymin,
            y1 = df_lists$col03$ymax,
            x0 = df_lists$col03$xmin,
            x1 = df_lists$col03$xmax,
            fillcolor = df_lists$col03$col,
            layer = "below"
          ),
          list(
            type = "rect",
            line = list(width = 0),
            y0 = df_lists$col04$ymin,
            y1 = df_lists$col04$ymax,
            x0 = df_lists$col04$xmin,
            x1 = df_lists$col04$xmax,
            fillcolor = "#00FF00",
            layer = "below"
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
          hoverinfo = "skip"
        ) %>%
        add_segments(
          x = 1,
          xend = 1,
          y = y_lim[1], 
          yend = y_lim[2],
          line = list(
            color = "black",
            width = 2,
            dash = "20px,10px"
          ),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "95%"),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[1],
          line = list(color = "black"),
          name = "95%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "80%"),
          x = ~x, 
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[2],
          line = list(color = "black"),
          name = "80%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = filter(ci_data, q == "50%"),
          x = ~x, 
          y = ~y,
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = palette[3],
          line = list(color = "black"),
          name = "50%",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = line_data,
          x = ~Bratio,
          y = ~Fratio,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 1),
          name = "Trajectories",
          hoverinfo = "text",
          text = ~paste0(
            "Year: ", year, "<br>B/B<sub>MSY</sub>: ", 
            .international_system_prefixes(Bratio), "<br>F/F<sub>MSY</sub>: ", 
            .international_system_prefixes(Fratio)
          )
        ) %>%
        add_markers(
          data = marker_data,
          x = ~Bratio,
          y = ~Fratio,
          symbol = ~factor(year),
          size = 10,
          marker = list(
            color = "white",
            line = list(color = "black")
          ),
          name = "Years",
          hoverinfo = "none"
        ) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            tickfont = list(size = 16),
            title = list(font = list(size = 20)),
            range = x_lim,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            tickfont = list(size = 16),
            title = list(font = list(size = 20)),
            range = y_lim,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          # hovermode = "x unified",
          hoverdistance = -1,
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
    }) %>% flatten()

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
            text = "B//Bmsy",
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
            text = "F/Fmsy",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$priors_posteriors_K <- renderPlotly({
    if (identical(pp_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- pp_df

    palette <- .resolve_palette(NULL, 2)

    scenarios <- unique(df_lists$prior$Scenario)

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

    prior_x_max <- max(prior$K01, na.rm = TRUE)
    pos_x_max <- max(posterior$K01, na.rm = TRUE)
    x_lim <- c(0, ifelse(prior_x_max > pos_x_max, prior_x_max, pos_x_max))

    max_y_pos <- .round_to_nearest(max(posterior$K02, na.rm = TRUE), TRUE, 1.1)
    min_y_pos <- .round_to_nearest(min(posterior$K02, na.rm = TRUE), FALSE, 1.1)

    max_prior <- .round_to_nearest(max(prior$K02, na.rm = TRUE), TRUE, 1.1)
    min_prior <- .round_to_nearest(min(prior$K02, na.rm = TRUE), FALSE, 1.1)

    max_y_val <- if (max_y_pos > max_prior) {
      max_y_pos
    }
    else {
      max_prior
    }

    min_y_val <- if (min_y_pos < min_prior) {
      min_y_pos
    }
    else {
      min_prior
    }
    y_lim <- c(min_y_val, max_y_val)

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
          textfont = list(size = 16),
          textposition = "top center"
        ) %>%
        add_text(
          data = PPVR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPVR = ", K),
          textfont = list(size = 16),
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
            text = "Carrying capacity (K)",
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
            text = "Density",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$priors_posteriors_r <- renderPlotly({
    if (identical(pp_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- pp_df

    palette <- .resolve_palette(NULL, 2)

    scenarios <- unique(df_lists$prior$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    prior <- df_lists$prior %>%
      select(Scenario, r01, r02)

    posterior <- df_lists$posterior %>%
      select(Scenario, r01, r02)

    prior_x_max <- max(prior$r01, na.rm = TRUE)
    pos_x_max <- max(posterior$r01, na.rm = TRUE)
    x_lim <- c(0, ifelse(prior_x_max > pos_x_max, prior_x_max, pos_x_max))

    max_y_pos <- .round_to_nearest(max(posterior$r02, na.rm = TRUE), TRUE, 1.1)
    min_y_pos <- .round_to_nearest(min(posterior$r02, na.rm = TRUE), FALSE, 1.1)

    max_prior <- .round_to_nearest(max(prior$r02, na.rm = TRUE), TRUE, 1.1)
    min_prior <- .round_to_nearest(min(prior$r02, na.rm = TRUE), FALSE, 1.1)

    max_y_val <- if (max_y_pos > max_prior) {
      max_y_pos
    }
    else {
      max_prior
    }

    min_y_val <- if (min_y_pos < min_prior) {
      min_y_pos
    }
    else {
      min_prior
    }
    y_lim <- c(min_y_val, max_y_val)

    pos <- .auto_text_position(
      data_list = list(prior, posterior), 
      col_x = "r01", 
      col_y = "r02",
      xlim = x_lim,
      ylim = y_lim, 
      margin = 0.05
    )

    PPMR <- df_lists$PPMR %>%
      select(Scenario, r) %>%
      mutate(x = pos$x, y = pos$y)


    PPVR <- df_lists$PPVR %>%
      select(Scenario, r) %>%
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
            "Prior<br>r01: ", .international_system_prefixes(r01), 
            "<br>r02: ", .international_system_prefixes(r02)
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
            "Posterior<br>r01: ", .international_system_prefixes(r01), 
            "<br>r02: ", .international_system_prefixes(r02)
          )
        ) %>%
        add_text(
          data = PPMR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPMR = ", r),
          textfont = list(size = 16),
          textposition = "top left"
        ) %>%
        add_text(
          data = PPVR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPVR = ", r),
          textfont = list(size = 16),
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
            text = "Intrisic growth rate (r)",
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
            text = "Density",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$priors_posteriors_psi <- renderPlotly({
    if (identical(pp_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- pp_df

    palette <- .resolve_palette(NULL, 2)

    scenarios <- unique(df_lists$prior$Scenario)

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

    prior_x_max <- max(prior$psi01, na.rm = TRUE)
    pos_x_max <- max(posterior$psi01, na.rm = TRUE)
    x_lim <- c(0, ifelse(prior_x_max > pos_x_max, prior_x_max, pos_x_max))

    max_y_pos <- .round_to_nearest(max(posterior$psi02, na.rm = TRUE), TRUE, 1.1)
    min_y_pos <- .round_to_nearest(min(posterior$psi02, na.rm = TRUE), FALSE, 1.1)

    max_prior <- .round_to_nearest(max(prior$psi02, na.rm = TRUE), TRUE, 1.1)
    min_prior <- .round_to_nearest(min(prior$psi02, na.rm = TRUE), FALSE, 1.1)

    max_y_val <- if (max_y_pos > max_prior) {
      max_y_pos
    }
    else {
      max_prior
    }

    min_y_val <- if (min_y_pos < min_prior) {
      min_y_pos
    }
    else {
      min_prior
    }
    y_lim <- c(min_y_val, max_y_val)

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
          textfont = list(size = 16),
          textposition = "top left"
        ) %>%
        add_text(
          data = PPVR,
          x = ~x,
          y = ~y,
          text = ~paste0("PPVR = ", psi),
          textfont = list(size = 16),
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
            text = "Initial biomass depletion ratio (psi)",
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
            text = "Density",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_B <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "B", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var[data_var$teste == TRUE, ]
    rho_var <- rho_data[rho_data$Index == "B", ]

    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(max(data_ref$Year), max(data_var$Year))
    min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.05
    )

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
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = 16),
          textposition = "bottom left",
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
            text = "Year",
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
            text = "Biomass (t)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_F <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "F", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var[data_var$teste == TRUE, ]
    rho_var <- rho_data[rho_data$Index == "F", ]

    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(max(data_ref$Year), max(data_var$Year))
    min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.05
    )

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
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = 16),
          textposition = "middle center",
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
            text = "Year",
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
            text = "Fishing Mortality (F)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_BBmsy <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "BBmsy", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var[data_var$teste == TRUE, ]
    rho_var <- rho_data[rho_data$Index == "BBmsy", ]

    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(max(data_ref$Year), max(data_var$Year))
    min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.05
    )

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
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = 16),
          textposition = "middle center",
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
            text = "Year",
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
            text = "B/Bmsy",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_FFmsy <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "FFmsy", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var[data_var$teste == TRUE, ]
    rho_var <- rho_data[rho_data$Index == "FFmsy", ]

    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(max(data_ref$Year), max(data_var$Year))
    min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.05
    )

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
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = 16),
          textposition = "middle center",
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
            text = "Year",
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
            text = "F/Fmsy",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_procB <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "procB", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var[data_var$teste == TRUE, ]
    rho_var <- rho_data[rho_data$Index == "procB", ]

    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(max(data_ref$Year), max(data_var$Year))
    min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.05
    )

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
        add_text(
          data = rho_var,
          x = pos$x,
          y = pos$y,
          text = ~paste0("\u03c1= ", .international_system_prefixes(rho, 2)),
          textfont = list(size = 16),
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
            text = "Year",
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
            text = "Process error on log(Biomass)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$retrospective_analysis_MSY <- renderPlotly({
    if (identical(ra_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }
    data <- ra_df$surplus_data

    scenarios <- unique(data$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, length(scenarios))

    rho_data <- ra_df$rho_data
    data_var <- data[data$Index == "MSY", ]

    data_ref <- data_var[data_var$id == "Ref", ]
    data_lines <- data_var
    rho_var <- rho_data[rho_data$Index == "MSY", ]

    max_y_val <- .round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)

    max_x_val <- max(max(data_ref$SB_i), max(data_var$SB_i))
    min_x_val <- min(min(data_ref$SB_i), min(data_var$SB_i))
    x_lim <- c(min_x_val, max_x_val)

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

      data_lines <- data_lines[!is.na(data_lines$SB_i) & !is.na(data_lines$SP), ]

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
          textfont = list(size = 16),
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
            text = "Biomass (t)",
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
            text = "Surplus Production (t)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$runs_tests <- renderPlotly({
    if (identical(res_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df_lists <- res_df
    
    max_y_val <- .round_to_nearest(max(df_lists$SE3$ucl, na.rm = TRUE), TRUE, 2.5)
    min_y_val <- .round_to_nearest(min(df_lists$SE3$lcl, na.rm = TRUE), FALSE, 2.5)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df_lists$SE3$ymax)
    min_x_val <- min(df_lists$SE3$ymin)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)

    scenarios <- unique(df_lists$cpue_residuals$Scenario)
    indices <- levels(df_lists$cpue_residuals$Index)

    n_scenarios <- length(scenarios)
    n_indices <- length(indices)

    pos <- .auto_text_position(
      data_list = df_lists$cpue_residuals,
      col_x = "Year",
      col_y = "Res",
      margin = 0.4,
      xlim = x_lim,
      ylim = y_lim
    )

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
            add_text(
              data = SE3,
              x = pos$x,
              y = pos$y,
              text = ~paste0("p-value = ", pvalue),
              textfont = list(size = 16),
              hoverinfo = "skip"
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
            )
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

    subplot(
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
            text = "Year",
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
            text = "Residuals",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_BB0 <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
      filter(indicator == "BB0")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
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
            text = "Year",
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
            text = "B/B0",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_BBmsy <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
      filter(indicator == "BBmsy")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0("Value: ", .international_system_prefixes(mu, 2))
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
        add_segments(
          x = x_lim[1],
          xend = x_lim[2],
          y = 0.4, 
          yend = 0.4,
          line = list(
            color = "red",
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
            text = "Year",
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
            text = "B/Bmsy",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_FFmsy <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
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

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0("Value: ", .international_system_prefixes(mu, 2))
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
            text = "Year",
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
            text = "F/Fmsy",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_Bdev <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
      filter(indicator == "Bdev")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
          hoverinfo = "text+x",
          text = ~paste0("Value: ", .international_system_prefixes(mu, 2))
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
            text = "Year",
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
            text = "Process Error on log(Biomass)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_B <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
      filter(indicator == "B")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
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
            text = "Year",
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
            text = "Biomass (t)",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_H <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
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

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
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
            text = "Year",
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
            text = "Harvest rate",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })

  output$trajectories_Catch <- renderPlotly({
    if (identical(traj_df, list())) {
      return(.empty_plotly("There is no data for this plot"))
    }

    df <- traj_df %>%
      filter(indicator == "Catch")

    scenarios <- unique(df$Scenario)

    n_scenarios <- length(scenarios)

    nrow <- if (n_scenarios < 3) {
      1
    } else if (n_scenarios < 8) {
      2
    } else {
      3
    }

    palette <- .resolve_palette(NULL, 1)
  
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)

    y_lim <- .expand_range(y_lim)
    x_lim <- .expand_range(x_lim)
    

    plots <- map(scenarios, function(s) {
      df <- df %>%
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
        add_ribbons(
          data = df,
          x = ~year,
          ymin = ~lcl2,
          ymax = ~ucl2,
          fillcolor = palette[1],
          opacity = 0.3,
          line = list(width = 0),
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
          type = "scatter",
          mode = "lines",
          line = list(width = 3, color = "black"),
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
            text = "Year",
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
            text = "Catch",
            showarrow = FALSE,
            font = list(
              size = 20
            )
          )
        )
      )
  })
}
