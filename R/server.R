#' @keywords internal
.build_server <- function(fits_df, hind_df, pp_df, res_df, kobe_df, traj_df, ra_df) {
  function(input, output, session) {

    .cpue_res_server(input, output, session, res_df)

    .fits_server(input, output, session, fits_df)
    
    .hindcast_server(input, output, session, hind_df)
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

    .priors_posteriors_K_server(input, output, session, pp_df)

    .priors_posteriors_r_server(input, output, session, pp_df)
    
    .priors_posteriors_psi_server(input, output, session, pp_df)

    .retrospective_analysis_B_server(input, output, session, ra_df)

    .retrospective_analysis_F_server(input, output, session, ra_df)

    .retrospective_analysis_BBmsy_server(input, output, session, ra_df)

    .retrospective_analysis_FFmsy_server(input, output, session, ra_df)
    
    .retrospective_analysis_procB_server(input, output, session, ra_df)

    .retrospective_analysis_MSY_server(input, output, session, ra_df)

    .runs_tests_server(input, output, session, res_df)

    .traj_BB0_server(input, output, session, traj_df)

    .traj_BBmsy_server(input, output, session, traj_df)
    
    .traj_FFmsy_server(input, output, session, traj_df)
    
    .traj_Bdev_server(input, output, session, traj_df)

    .traj_B_server(input, output, session, traj_df)
    
    .traj_H_server(input, output, session, traj_df)

    .traj_Catch_server(input, output, session, traj_df)
    }
}