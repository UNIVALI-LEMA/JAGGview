#' Plot CPUE residuals diagnostics
#' 
#' Creates a ggplot2- based visualization of CPUE residuals across
#' years and scenarios, including reference lines, residual segments,
#' smoothed trends, and RMSE annotations.
#' 
#' @param df_lists A named list as returned by \code{runs_tests_data()}. It 
#'   must contain \code{cpue_residuals}, \code{SE3}, \code{RMSE_data}.
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param position A character string specifying the table's position within 
#'   each plot panel, combining a vertical and a horizontal keyword separated 
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical 
#'   component must be one of \code{"top"}, \code{"middle"}, or 
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"}, 
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"}, 
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"}, 
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"}, 
#'   \code{"bottom-center"}, and \code{"bottom-right"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. 
#'   Defaults to "Residuals"
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL} (default), a color-blind-friendly palette is generated
#'   automatically according to the number of index levels.
#'   If the number of suplied colors is smaller than the number specified, 
#'   than the code returns an error.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#' 
#' @return A ggplot object displaying CPUE residual diagnostics.
#' 
#' @details 
#' The plot includes residual segments, observed values, a smoothed trend,
#' and RMSE annotations for each scenario.
#' 
#' @family visualization functions
#' @family cpue residuals runs tests functions
#' 
#' @export
#' @importFrom ggplot2 .pt aes facet_wrap geom_hline geom_point geom_segment 
#' geom_smooth ggplot labs scale_colour_manual scale_fill_manual 
#' scale_y_continuous theme
#' @importFrom grDevices colorRampPalette
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
cpue_residuals_ggplot <- function(
  df_lists, n_col = 3, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Residuals", palette = NULL, x_lim = NULL, y_lim = NULL
) {

  n_levels <- length(unique(df_lists$cpue_residuals$Index))

  palette <- .resolve_palette(palette, n_levels)

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(
      df_lists$cpue_residuals$Res, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(
      df_lists$cpue_residuals$Res, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }
  
  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$cpue_residuals$Year, na.rm = TRUE)
    min_x_val <- min(df_lists$cpue_residuals$Year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)
  }
  
  table <- .prepare_npc_table_data(
    data = df_lists$RMSE_data, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = Value, 
    col_name = "RMSE", 
    suffix = "%"
  )
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(data = df_lists$cpue_residuals,
                 aes(x = Year, xend = Year, y = Ref, yend = Res,
                     colour = Index)) +
    geom_point(data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res, fill = Index, colour = Index),
               pch = 21, size = 2) +
    geom_smooth(data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res), se = TRUE, colour = "black") +
    geom_table_npc(data = table,
                  aes(npcx = x, npcy = y, label = tb), 
                  size = text_size,
                  table.theme = ttheme_gtdefault(base_size = text_size * .pt)) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = n_col) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    labs(x = title_x, y = title_y, fill = "", colour = "") +
    .my_theme() +
    theme(legend.position = "top")
}

#' Plot fitted indices with credibility intervals
#'
#' Creates a ggplot2-based visualization of fitted abundance indices,
#' including mean values and credibility intervals (80% and 95%).
#'
#' @param df_lists A named list of data frames as returned by
#'   \code{fits_data()}. It must contain the elements \code{Li_Ui},
#'   \code{CI_80}, and \code{CI_95}.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Abundance index".
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL} (default), a color-blind-friendly palette is generated
#'   automatically according to the number of index levels.
#'   If the number of suplied colors is smaller than the number specified, 
#'   than the code returns an error.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying fitted indices with uncertainty ribbons, 
#'   error bars, and observed values, faceted by scenario and index.
#'
#' @details
#' The plot includes ribbons representing 80% and 95% credibility intervals, a 
#' fitted line, observed points with error bars, and faceting by scenario and 
#' index.
#'
#' @examples
#' \dontrun{
#' df <- fits_data(list_fit_models)
#' fits_ggplot(df, palette = "blue")
#' }
#' 
#' @family visualization functions
#' @family fits functions
#'
#' @export
#' @importFrom ggplot2 aes coord_cartesian facet_grid geom_errorbar geom_line 
#' geom_point geom_ribbon ggplot labs
fits_ggplot <- function(
  df_lists, title_x = "Year", title_y = "Abundance index", palette = NULL, 
  x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  palette <- .resolve_palette(palette, 1)

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$CI_95$uci, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(df_lists$CI_95$lci, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$CI_80$Year, na.rm = TRUE)
    min_x_val <- min(df_lists$CI_80$Year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)
  }

  ggplot() +
    geom_ribbon(data = df_lists$CI_80,
        aes(x = Year, ymin = lci, ymax = uci),
        alpha = 0.3, fill = palette[1]) +
    geom_ribbon(data = df_lists$CI_95,
        aes(x = Year, ymin = lci, ymax = uci),
        alpha = 0.3, fill = palette[1]) +
    geom_line(data = df_lists$CI_80, aes(x = Year, y = mu)) +
    geom_errorbar(data = df_lists$Li_Ui, 
                  aes(x = Year, ymin = Li, ymax = Ui
                  ), width = 1.5) +
    geom_point(data = df_lists$Li_Ui,
        aes(x = Year, y = Mean),
        pch = 21, fill = "white", size = 1.5) + 
    facet_grid(Scenario ~ Index, scales = "free") +
    coord_cartesian(xlim = x_lim, ylim = y_lim) + 
    labs(x = title_x, y = title_y) +
    .my_theme()
}

#' Plot hindcast diagnostics
#'
#' Creates a ggplot2-based visualization of hindcast diagnostics, including 
#' observed and predicted values, uncertainty intervals, and model performance 
#' metrics (MASE).
#'
#' @param df_lists A named list as returned by \code{hindcast_data()}. It must
#'   contain the elements \code{data}, \code{data_points}, 
#'   \code{data_lines} and \code{mase_data}.
#' @param position A character string specifying the table's position within 
#'   each plot panel, combining a vertical and a horizontal keyword separated 
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical 
#'   component must be one of \code{"top"}, \code{"middle"}, or 
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"}, 
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"}, 
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"}, 
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"}, 
#'   \code{"bottom-center"}, and \code{"bottom-right"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to "Index".
#' @param zoom Optional. A boolean value that if \code{TRUE} shows a subplot of 
#'   a zoomed view of the hindcast window. Facets with no data for a given
#'   Scenario and Index combination are left blank. Defaults to \code{FALSE}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying hindcast trajectories, observed data 
#'   points, uncertainty ribbons, and MASE annotations, faceted by scenario and 
#'   index.
#'
#' @details
#' The plot includes credibility ribbons for reference runs, hindcast
#' trajectories, observed and predicted points, and annotations of MASE values. 
#' Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' df <- hindcast_data(list_hc_models)
#' hindcast_ggplot(df)
#' }
#' 
#' @family visualization functions
#' @family hindcasts functions
#'
#' @export 
#' @importFrom ggplot2 .pt aes coord_cartesian facet_wrap geom_line geom_point 
#' geom_rect geom_ribbon ggplot guide_legend guides labs scale_colour_manual 
#' scale_fill_manual theme
#' @importFrom dplyr filter mutate select vars
#' @importFrom JABBA ss3col
#' @importFrom ggpp geom_plot geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
hindcast_ggplot <- function(
  df_lists, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Index", zoom = FALSE, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), 
    TRUE)
    min_y_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), 
    FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$data$year)
    min_x_val <- min(df_lists$data$year)
    x_lim <- c(min_x_val, max_x_val)
  }

  x_min_zoom <- x_lim[2] - (x_lim[2] - x_lim[1]) * 0.5
  y_min_zoom <- y_lim[2] - (y_lim[2] - y_lim[1]) * 0.5

  zoom_x_lim <- if(!zoom) x_lim else c(x_lim[1], x_min_zoom)
  
  table <- .prepare_npc_table_data(
    data = df_lists$mase_data, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = MASE, 
    col_name = "MASE", 
    suffix = "%", 
    decimals = 3
  )

  min_year_hc <- min(df_lists$data_lines$year) - 1

  max_year_hc <- max(df_lists$data_lines$year)
  
  p1 <- ggplot() +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0),
        aes(x = year,
            ymin = hat.lci, ymax = hat.uci),
        fill = "gray80") +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0, 
                              year < df_lists$min_year_retro),
                aes(x = year, ymin = hat.lci, ymax = hat.uci),
                fill = "gray30", alpha = 0.5) +
    geom_line(data = filter(df_lists$data, hindcast == FALSE),
              aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = df_lists$data_lines,
              aes(x = year, y = hat, group = retro.peels),
              linewidth = 1, colour = "white") +
    geom_point(data = filter(df_lists$data, retro.peels == 0, 
                            year < df_lists$min_year_retro),
               aes(x = year, y = obs), pch = 21, size = 4,
               fill = "white") +
    geom_point(data = df_lists$data_points, show.legend = FALSE,
               aes(x = year, y = obs, fill = retro),
               pch = 21, size = 4) +
    geom_point(data = df_lists$data_points, show.legend = FALSE,
               aes(x = year, y = hat, fill = retro),
               pch = 21, size = 2) +
    geom_table_npc(data = table,
                  aes(npcx = x, npcy = y, label = tb),
                  size = text_size,
                  table.theme = ttheme_gtdefault(base_size = text_size * .pt)) +
    labs(x = title_x, y = title_y, colour = "") +
    facet_wrap(Scenario ~ Index, ncol = length(unique(df_lists$data$Index)), 
              drop = FALSE) +
    facet_grid(rows = vars(Scenario), cols = vars(Index)) +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    .my_theme() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))

  if (!zoom) {
    return(p1)
  } 

  scenarios <- unique(df_lists$data$Scenario)
  indices <- unique(df_lists$data$Index)

  combos <- expand.grid(
    Scenario = scenarios,
    Index = indices,
    stringsAsFactors = FALSE
  )

  combos$plot <- Map(
    .make_zoom_plot, 
    combos$Scenario, 
    combos$Index,
    MoreArgs = list(
      df = df_lists,
      min_year = min_year_hc,
      max_year = max_year_hc
    )
  )

  combos_valid <- combos %>% filter(!sapply(plot, is.null))

  combos_valid$x <- x_lim[2]
  combos_valid$y <- y_lim[2]

  zoom_ribbon_all <- df_lists$data %>%
    filter(year >= min_year_hc, retro.peels == 0)

  min_y_zoom_ribbon <- floor(min(zoom_ribbon_all$hat.lci) * 10) / 10
  max_y_zoom_ribbon <- ceiling(max(zoom_ribbon_all$hat.uci) * 10) / 10

  min_y_zoom_points_1 <- floor(min(df_lists$data_points$obs) * 10) / 10
  max_y_zoom_points_1 <- ceiling(max(df_lists$data_points$obs) * 10) / 10

  min_y_zoom_points_2 <- floor(min(df_lists$data_lines$obs) * 10) / 10
  max_y_zoom_points_2 <- ceiling(max(df_lists$data_lines$obs) * 10) / 10

  min_y_zoom <- min(min_y_zoom_ribbon, min_y_zoom_points_1, min_y_zoom_points_2)
  max_y_zoom <- max(max_y_zoom_ribbon, max_y_zoom_points_1, max_y_zoom_points_2)

  rect_data <- combos_valid %>%
    select(Scenario, Index) %>%
    mutate(
      xmin = min_year_hc, xmax = max_year_hc, 
      ymin = min_y_zoom,  ymax = max_y_zoom
    )

  p3 <- p1 +
    geom_rect(
      data = rect_data,
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA, colour = "black", linetype = 2, linewidth = 0.5, 
      inherit.aes = FALSE
    ) +
    geom_plot(
      data = combos_valid,
      mapping = aes(x = x, y = y, label = plot),
      vp.width  = 0.45,
      vp.height = 0.45,
      hjust = 1,
      vjust = 1
    )

  p3
}

#' Plot Kobe diagram
#'
#' Creates a Kobe plot showing the status of fish stocks in terms of biomass 
#' (B/Bmsy) and fishing mortality (F/Fmsy), including uncertainty contours and 
#' temporal trajectories.
#'
#' @param df_lists A named list as returned by \code{kobe_data()}. It must 
#'   contain the elements \code{col01}, \code{col02}, \code{col03}, 
#'   \code{col04}, \code{ci_data}, \code{data_lines} and \code{highlight_years}.
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param title_x A character string for the x-axis label. Defaults to "B/Bmsy".
#' @param title_y A character string for the y-axis label. Defaults to "F/Fmsy".
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object representing the Kobe plot, faceted by scenario.
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Colored quadrants representing stock status regions.
#'   \item Kernel density contours (50%, 80%, 95%) for uncertainty.
#'   \item Time series trajectory of stock status.
#'   \item Highlighted reference years.
#'   \item Reference lines at B/Bmsy = 1 and F/Fmsy = 1.
#' }
#'
#' Faceting is applied by scenario, allowing comparison across model runs.
#'
#' @examples
#' \dontrun{
#' df <- kobe_data(model_results)
#' kobe_ggplot(df)
#' }
#' 
#' @family visualization functions
#' @family kobe functions
#'
#' @export
#' @importFrom ggplot2 aes coord_cartesian facet_wrap geom_hline geom_path 
#' geom_point geom_polygon geom_rect geom_vline ggplot labs scale_fill_manual 
#' scale_shape_manual scale_x_continuous scale_y_continuous theme
#' @importFrom grDevices colorRampPalette
kobe_ggplot <- function(
  df_lists, n_col = 3, title_x = expression(B/B[MSY]), 
  title_y = expression(F/F[MSY]), x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y <- df_lists$col02$ymax
    y_lim <- c(0, max_y)
  }
  if (is.null(x_lim)) {
    max_x <- df_lists$col02$xmax
    x_lim <- c(0, max_x)
  }
  if (x_lim[1] < 0) x_lim[1] <- 0
  if (y_lim[1] < 0) y_lim[1] <- 0

  n_levels <- length(unique(df_lists$ci_data$q))
  ggplot() +
    geom_rect(data = df_lists$col01, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "yellow") +
    geom_rect(data = df_lists$col02, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "orange") +
    geom_rect(data = df_lists$col03, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "red") +
    geom_rect(data = df_lists$col04, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "green") +
    geom_hline(yintercept = 1, linetype = "longdash") +
    geom_vline(xintercept = 1, linetype = "longdash") +
    geom_polygon(data = df_lists$ci_data,
                 aes(x = x, y = y, fill = q),
                 colour = "gray30") +
    geom_path(data = df_lists$data_lines, aes(x = Bratio, y = Fratio)) +
    geom_point(data = df_lists$highlight_years,
               aes(x = Bratio, y = Fratio, shape = factor(year)),
               size = 4, fill = "white") +
    facet_wrap(~ Scenario, scales = "free_x", ncol = n_col) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, y_lim[2], 1)) + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, x_lim[2], 1)) + 
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(
      values = colorRampPalette(c("cornsilk4", "grey", "cornsilk2"))(n_levels)
    ) +
    labs(x = title_x, y = title_y, fill = "", shape = "") +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    .my_theme() +
    theme(legend.position = "top")
}

#' Plot prior and posterior distributions
#'
#' Creates a ggplot2-based visualization comparing prior and posterior
#' distributions for a selected parameter across scenarios.
#'
#' @param df_lists A named list as returned by \code{priors_posteriors_data()}. 
#'   It must contain the elements \code{prior}, \code{posterior}, \code{PPVR} 
#'   and \code{PPMR}.
#' @param indicator_name A character string specifying the parameter to plot.
#'   Supported values include "K", "r", and "psi".
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param position A character string specifying the table's position within 
#'   each plot panel, combining a vertical and a horizontal keyword separated 
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical 
#'   component must be one of \code{"top"}, \code{"middle"}, or 
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"}, 
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"}, 
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"}, 
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"}, 
#'   \code{"bottom-center"}, and \code{"bottom-right"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Density".
#' @param use_si_suffix A boolean value indicating whether SI suffixes will be 
#'   used, or if FALSE then shows the absolute number, Defaults to FALSE.
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL} (default), a color-blind-friendly palette is generated
#'   automatically according to the number of index levels.
#'   If the number of suplied colors is smaller than the number specified, 
#'   than the code returns an error.
#' @param title_x A character string for the x-axis label. If \code{NULL}, a 
#'   default label is assigned based on \code{indicator_name}.
#' @param x_decimals Optional. Number of decimal places.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying prior and posterior densities, annotated 
#'   with prior-posterior metrics (PPMR and PPVR).
#'
#' @details
#' The plot overlays prior and posterior density curves, includes annotations 
#' for prior-posterior mean and variance ratios, and faceted views by scenario.
#'
#' @examples
#' \dontrun{
#' df <- priors_posteriors_data(list_fit_models)
#' priors_posteriors_ggplot(
#'   df, "K", use_si_suffix  TRUE, palette = c("#4285f4", "#34a853")
#' )
#' }
#' 
#' @family visualization functions
#' @family priors vs posteriors functions
#'
#' @export
#' @importFrom dplyr %>% all_of filter full_join pull rename select
#' @importFrom ggplot2 .pt aes coord_cartesian element_blank facet_wrap 
#' geom_area ggplot labs scale_x_continuous scale_y_continuous theme
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
priors_posteriors_ggplot <- function(
  df_lists, indicator_name, n_col = 3, position = "top-left", text_size = 6, 
  title_y = "Density", use_si_suffix = FALSE, palette = NULL, title_x = NULL, 
  x_decimals = NULL, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if (!indicator_name %in% c("K", "r", "psi")) {
    stop("Parameter 'indicator_name' was expecting 'K', 'r' or 'psi'.")
  }

  palette <- .resolve_palette(palette, 2)

  .axis_limit(x_lim)

  .axis_limit(y_lim)

  indicator1 <- paste0(indicator_name, "01")
  indicator2 <- paste0(indicator_name, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(
      value_1 = all_of(indicator1),
      value_2 = all_of(indicator2)
    )
  
  labels_x <- list(
    K = "Carrying capacity (K)",
    r = "Intrinsic growth rate (r)",
    psi = "Initial biomass depletion ratio (psi)"
  )

  if (is.null(title_x)) {
    title_x <- labels_x[[indicator_name]]
  }
  
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(
      value_1 = all_of(indicator1),
      value_2 = all_of(indicator2)
    )
  
  if (is.null(x_lim)) {
    prior_x_max <- max(prior$value_1, na.rm = TRUE)
    pos_x_max <- max(posterior$value_1, na.rm = TRUE)
    x_lim <- c(0, ifelse(prior_x_max > pos_x_max, prior_x_max, pos_x_max))
  }

  if (is.null(x_decimals)) {
    x_decimals <- ifelse(x_lim[2] > 10, 0, 2)
  }

  if (is.null(y_lim)) {
    max_y_pos <- .round_to_nearest(max(posterior$value_2, na.rm = TRUE), 
                                    TRUE, 1.1)
    min_y_pos <- .round_to_nearest(min(posterior$value_2, na.rm = TRUE), 
                                    FALSE, 1.1)

    max_prior <- .round_to_nearest(max(prior$value_2, na.rm = TRUE), 
                                  TRUE, 1.1)
    min_prior <- .round_to_nearest(min(prior$value_2, na.rm = TRUE), 
                                  FALSE, 1.1)

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
  }
  
  df_text <- df_lists$PPMR %>%
  select(Scenario, ppmr_value = all_of(indicator_name)) %>%
  full_join(
    df_lists$PPVR %>%
      select(Scenario, ppvr_value = all_of(indicator_name)),
    by = "Scenario"
  ) 

  table <- .prepare_npc_table_data(
    data = df_text,
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = c(ppmr_value, ppvr_value), 
    col_name = c("PPMR", "PPVR"),
    decimals = 3
  )

  x_labels <- if (use_si_suffix) {
    function(x) .international_system_prefixes(x)
  } else {
    function(x) {
      format(x, nsmall = x_decimals, scientific = FALSE, 
        big.mark = ".", decimal.mark = ",")
    }
  }
  
  ggplot() +
    geom_area(data = prior, aes(x = value_1, y = value_2),
              fill = palette[1], alpha = 0.5, colour = "black") +
    geom_area(data = posterior, aes(x = value_1, y = value_2),
              fill = palette[2], alpha = 0.5, colour = "black") +
    geom_table_npc(data = table,
                  aes(npcx = x, npcy = y, label = tb),
                  size = text_size,
                  table.theme = ttheme_gtdefault(base_size = text_size * .pt)) +
    facet_wrap(~Scenario, ncol = n_col) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = title_y) +
    scale_x_continuous(labels = x_labels) +
    scale_y_continuous(expand = c(0, 0)) +
    .my_theme() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

#' Plot retrospective analysis results
#'
#' Creates a ggplot2-based visualization of retrospective analyses, including 
#' time series of key indices and surplus production curves, along with 
#' retrospective bias (rho) annotations.
#'
#' @param df_lists A named list as returned by
#'   \code{retrospective_analysis_data()}. It must contain the elements 
#'   \code{data}, \code{surplus_data} and \code{rho_data}.
#' @param indicator_name A character string specifying the name of the 
#'   indicator to plot. Supported values include "B", "F", "BBmsy", "FFmsy",
#'   "procB", and "MSY".
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param position A character string specifying the table's position within 
#'   each plot panel, combining a vertical and a horizontal keyword separated 
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical 
#'   component must be one of \code{"top"}, \code{"middle"}, or 
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"}, 
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"}, 
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"}, 
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"}, 
#'   \code{"bottom-center"}, and \code{"bottom-right"}.
#' @param use_si_suffix A boolean value indicating whether SI suffixes will be 
#'   used, or if FALSE then shows the absolute number, Defaults to FALSE.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_x A character string for the x-axis label. If \code{NULL}, a 
#'   default label is assigned based on \code{indicator_name}.
#' @param title_y A character string for the y-axis label. If \code{NULL}, a 
#'   default label is assigned based on \code{indicator_name}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying retrospective trajectories, credibility 
#' intervals (when applicable), and rho annotations.
#'
#' @details
#' For standard indices, the plot shows time series with credibility ribbons 
#' and retrospective trajectories. For "MSY", the plot displays surplus 
#' production curves as a function of biomass. Results are faceted by scenario.
#'
#' @examples
#' \dontrun{
#' df <- retrospective_analysis_data(list_hc_models)
#' retrospective_analysis_ggplot(df, indicator_name = "B")
#' }
#' 
#' @family visualization functions
#' @family retrospective analysis functions
#'
#' @export
#' @importFrom ggplot2 .pt aes coord_cartesian element_text facet_wrap 
#' geom_hline geom_line geom_ribbon ggplot guide_legend guides labs 
#' scale_colour_manual scale_x_continuous scale_y_continuous theme
#' @importFrom JABBA ss3col
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
retrospective_analysis_ggplot <- function(
  df_lists, indicator_name, n_col = 3, position = "top-left", text_size = 6, 
  use_si_suffix = FALSE, title_x = NULL, title_y = NULL, x_lim = NULL, 
  y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if (!indicator_name %in% c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")) {
    stop(paste0(
      "Parameter 'indicator_name' was expecting 'B', 'F', 'BBmsy', 'FFmsy', ",
      "'procB' or 'MSY'."
    ))
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (indicator_name != "MSY") {
    data <- df_lists$data
    if (is.null(title_x)) title_x <- "Year"
  } else {
    data <- df_lists$surplus_data
    if (is.null(title_x)) title_x <- "Biomass (t)"
  }

  labels_y <- list(
    B = "Biomass (t)",
    F = "Fishing Mortality (F)",
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY]),
    procB = "Process error on log(Biomass)",
    MSY = "Surplus Production (t)"
  )

  if (is.null(title_y)) {
    title_y <- labels_y[[indicator_name]]
  }
  
  rho_data <- df_lists$rho_data
  data_var <- data[data$Index == indicator_name, ]
  
  data_ref   <- data_var[data_var$id == "Ref", ]
  if (indicator_name != "MSY") {
    data_lines <- data_var[data_var$teste == TRUE, ]
  } else {
    data_lines <- data_var
  }
  rho_var <- rho_data[rho_data$Index == indicator_name, ]
  
  if (indicator_name != "MSY") {
    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }
    if (is.null(x_lim)) {
      max_x_val <- max(max(data_ref$Year), max(data_var$Year))
      min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    }
  } else {
    max_y_val <- .round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }

    if (is.null(x_lim)) {
      max_x_val <- max(max(data_ref$SB_i), max(data_var$SB_i))
      min_x_val <- min(min(data_ref$SB_i), min(data_var$SB_i))
      x_lim <- c(min_x_val, max_x_val)
    }

    max_x_val <- .round_to_nearest(max(data_ref$SB_i, na.rm = TRUE), TRUE, 1.1)
    x_decimals <- ifelse(x_lim[2] > 10, 0, 1)
    x_labels <- if (use_si_suffix) {
      function(x) .international_system_prefixes(x)
    } else {
      function(x) {
        format(x, nsmall = x_decimals, scientific = FALSE, 
          big.mark = ".", decimal.mark = ",")
      }
    }
  }
  table <- .prepare_npc_table_data(
    data = rho_var, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = rho, 
    col_name = "rho", 
    decimals = 3
  )

  y_decimals <- ifelse(y_lim[2] > 10, 0, 1)

  y_labels <- if (use_si_suffix) {
    function(x) .international_system_prefixes(x)
  } else {
    function(x) {
      format(x, nsmall = y_decimals, scientific = FALSE, 
        big.mark = ".", decimal.mark = ",")
    }
  }
  
  p <- ggplot()
  
  if (indicator_name != "MSY") {
    p <- p +
      geom_ribbon(
        data = data_ref,
        aes(x = Year, ymin = lci, ymax = uci),
        fill = "gray80"
      ) +
      geom_line(
        data = data_lines,
        aes(x = Year, y = mu, colour = id, group = id),
        linewidth = 1
      )
    if (indicator_name %in% c("BBmsy", "FFmsy")) {
      p <- p +
        geom_hline(yintercept = 1, linetype = "longdash")
    } 
    else if (indicator_name == "procB") {
      p <- p +
        geom_hline(yintercept = 0, linetype = "longdash")
    }  
  } 
  else {
    data_lines <- data_lines[!is.na(data_lines$SB_i) & !is.na(data_lines$SP), ]
    
    p <- p +
      geom_line(
        data = data_lines,
        aes(x = SB_i, y = SP, colour = id, group = id),
        linewidth = 1
      )
  }
  
  p <- p +
    geom_table_npc(data = table,
                  aes(npcx = x, npcy = y, label = tb), 
                  size = text_size,
                  table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
    facet_wrap(~Scenario, ncol = n_col, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim)

  if (indicator_name == "MSY") {
    p <- p +
      scale_x_continuous(labels = x_labels)
  }
  
  p <- p +
    labs(x = title_x, y = title_y, colour = "") +
    .my_theme() +
    theme(
      legend.position = "bottom",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    ) +
    guides(colour = guide_legend(nrow = 1))
  p
}

#' Plot runs test diagnostics
#'
#' Creates a ggplot2-based visualization of runs test diagnostics, including 
#' residuals, credibility limits, and p-values across scenarios and indices.
#'
#' @param df_lists A named list as returned by \code{runs_tests_data()}. It 
#'   must contain \code{cpue_residuals}, \code{SE3}, \code{RMSE_data}.
#' @param position A character string specifying the table's position within 
#'   each plot panel, combining a vertical and a horizontal keyword separated 
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical 
#'   component must be one of \code{"top"}, \code{"middle"}, or 
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"}, 
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"}, 
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"}, 
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"}, 
#'   \code{"bottom-center"}, and \code{"bottom-right"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Residuals".
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object showing residuals, credibility regions, and runs 
#'   test results.
#'
#' @details
#' The plot includes shaded regions representing credibility limits, residual 
#' segments, highlighted points based on threshold exceedance, and p-value 
#' annotations. Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' df <- runs_tests_data(list_fit_models)
#' runs_tests_ggplot(df)
#' }
#' 
#' @family visualization functions
#' @family cpue residuals runs tests functions
#'
#' @export
#' @importFrom ggplot2 .pt ggplot geom_rect aes geom_hline geom_segment
#' geom_point facet_grid scale_fill_manual labs theme
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
runs_tests_ggplot <- function(
  df_lists, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Residuals", x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$SE3$ucl, na.rm = TRUE), TRUE, 
    2.5)
    min_y_val <- .round_to_nearest(min(df_lists$SE3$lcl, na.rm = TRUE), FALSE, 
    2.5)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$SE3$ymax)
    min_x_val <- min(df_lists$SE3$ymin)
    x_lim <- c(min_x_val, max_x_val)
  }

  table <- .prepare_npc_table_data(
    data = df_lists$SE3, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = pvalue, 
    col_name = "p-value", 
    decimals = 3
  )

  ggplot() +
    geom_rect(data = df_lists$SE3,
              aes(xmin = ymin, xmax = ymax, ymin = lcl, ymax = ucl, 
                  fill = class),
              alpha = 0.2) +
    geom_table_npc(data = table,
                  aes(npcx = x, npcy = y,label = tb), 
                  size = text_size,
                  table.theme = ttheme_gtdefault(base_size = text_size * .pt)) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(data = df_lists$cpue_residuals,
                 aes(x = Year, xend = Year, y = Ref, yend = Res)) +
    geom_point(data = filter(df_lists$cpue_residuals, class == "white"),
        aes(x = Year, y = Res), fill = "white",
        pch = 21, size = 2) +
    geom_point(data = filter(df_lists$cpue_residuals, class == "red"),
        aes(x = Year, y = Res), fill = "red",
        pch = 21, size = 2.5) +
      facet_grid(Scenario ~ Index, scales = "free") +
    scale_fill_manual(values = c("green", "red")) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
}

#' Create and display a summary table
#' 
#' Generates a formatted table from data returned by \code{get_*()} acessor 
#' functions, using the \pkg{gt} package. The table can be displayed in the
#' Viewer pane, printed to the console, and optionally saved to file.
#' 
#' @param data A data frame containing extracted model results, returned by one 
#'   of the package \code{get_*()} functions.
#' @param show Optional. Character string specifying how the table should be
#'   displayed. Possible values are:
#'   \itemize{
#'     \item \code{"html"}: displays the table as an HTML file in the Viewer.
#'     \item \code{"png"}: displays the table as a PNG image in the Viewer.
#'     \item \code{"console"}: prints the raw data frame to the console.
#'     \item \code{NULL}: no display output.
#'   }
#' @param save Optional. Character string specifying the output format used to
#'   save the table. Possible values are:
#'   \itemize{
#'     \item \code{"html"}: saves the table as an HTML file.
#'     \item \code{"png"}: saves the table as a PNG image.
#'     \item \code{"pdf"}: saves the table as a PDF file.
#'     \item \code{"xlsx"}: saves the raw data as an Excel file.
#'     \item \code{NULL}: no file is saved.
#'   }
#' @param filename Optional. Character string specifying the output file name
#'   without extension. Defaults to \code{"summary_table"}.
#' @param digits A integer indicating the number of decimals places to display. 
#'   Defaults to 4.
#' 
#' @return
#' Invisibly returns a formatted \code{gt} table object.
#' 
#' @details
#' The generated table automatically formats column labels in bold and applies
#' alternating row background colors to improve readability.
#' 
#' When \code{show} is enabled, temporary files are generated and displayed
#' using the RStudio Viewer pane. If \code{show = "console"}, the raw input
#' data frame is printed instead of the formatted table.
#' 
#' If \code{save = "xlsx"}, the raw input data frame is exported using
#' \pkg{openxlsx}; otherwise, the formatted \pkg{gt} table is exported using
#' \code{gtsave()}.
#' 
#' This function is designed as a general-purpose table formatter for outputs
#' generated by package extraction functions.
#' 
#' @examples
#' \dontrun{
#' summary_table(get_mase(hindcast_data(list_hc_models)))
#' 
#' summary_table(get_ppmr(priors_posteriors_data(list_fit_models)), "png")
#' 
#' summary_table(get_pars(list_fit_models), "console", "xlsx", "pars_table")
#' }
#' 
#' @family visualization functions
#' 
#' @export
#' @importFrom gt gtsave
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom openxlsx write.xlsx
#' @importFrom utils browseURL
summary_table <- function(
  data, show = "html", save = NULL, filename = "summary_table", digits = 4
) {
  table <- .default_table(data, digits = digits)
  
  if (!is.null(show)) {
    ext <- switch(
      show,
      html = ".html",
      png  = ".png",
      console  = "console",
      stop("Invalid show type")
    )
    if (ext != "console") {
      file <- tempfile(fileext = ext)
      gtsave(table, file)
      if (isAvailable()) {
        viewer(file)
      } else {
        browseURL(file)
      }
    }
    else {
      print(data)
    }
  }

  if (!is.null(save)) {
    ext <- switch(
      save,
      png  = ".png",
      html = ".html",
      pdf  = ".pdf",
      xlsx = ".xlsx",
      stop("Invalid save type")
    )

    file <- paste0(filename, ext)
    if (ext != ".xlsx") {
      gtsave(table, file)
    }
    else {
      write.xlsx(data, file)
    }
  }
}

#' Plot model trajectories
#'
#' Creates a ggplot2-based visualization of model trajectories over time,
#' including median trends and uncertainty intervals across scenarios.
#'
#' @param df A data frame as returned by \code{trajectories_data()}.
#' @param indicator_name A character string indicating the indicator_name to 
#'   plot. Options are \code{"BB0"}, \code{"BBmsy"}, \code{"FFmsy"}, 
#'   \code{"Bdev"}, \code{"B"}, \code{"H"} or \code{"Catch"}.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param use_si_suffix A boolean value indicating whether SI suffixes will be 
#'   used, or if FALSE then shows the absolute number, Defaults to FALSE.
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL} (default), a color-blind-friendly palette is generated
#'   automatically according to the number of index levels.
#'   If the number of suplied colors is smaller than the number specified, 
#'   than the code returns an error.
#' @param title_y Optional. A character string or expression for the y-axis 
#'   label. Defaults to a predefined label depending on the selected variable.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying trajectory summaries with credibility 
#'   intervals, faceted by scenario.
#'
#' @details
#' The functions filters the input data based on the selected 
#' \code{indicator_name} (matching the \code{indicator} column). The plot 
#' includes ribbons representing 80% (\code{lcl2}-\code{ucl2}) and 95% 
#' (\code{lcl}-\code{ucl}) credibility intervals, as well as a median 
#' trajectory line (\code{mu}).
#'
#' Reference lines are added depending on the selected indicator_name:
#' \itemize{
#'   \item \code{"BBmsy"}: horizontal lines at 1 and 0.4
#'   \item \code{"FFmsy"}: horizontal line at 1
#'   \item \code{"Bdev"}: horizontal line at 0
#' }
#'
#' The y-axis limits are automatically adjusted based on the data range, and 
#' labels are formatted dynamically. The y-axis label is automatically defined 
#' unless provided by the user.
#' 
#' @examples
#' \dontrun{
#' df <- trajectories_data(out)
#' trajectories_ggplot(df, indicator_name = "BB0", palette = c("blue"))
#' }
#' 
#' @family visualization functions
#' @family trajectories functions
#'
#' @export
#' @importFrom ggplot2 aes coord_cartesian facet_wrap geom_hline geom_line 
#' geom_ribbon ggplot labs scale_y_continuous theme
trajectories_ggplot <- function(
  df, indicator_name, n_col = 3, title_x = "Year", use_si_suffix = FALSE, 
  palette = NULL, title_y = NULL, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if (!indicator_name %in% c(
    "BB0", "BBmsy", "FFmsy", "Bdev", "B", "H", "Catch"
  )) {
    stop(paste0(
      "Parameter 'indicator_name' was expecting 'BB0', 'BBmsy', 'FFmsy', ", 
      "'Bdev', 'B', 'H' or 'Catch'."
    ))
  }
  palette <- .resolve_palette(palette, 1)

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  df <- df %>%
    filter(indicator == indicator_name)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df$year, na.rm = TRUE)
    min_x_val <- min(df$year, na.rm = TRUE)
    x_lim <- c(min_x_val, max_x_val)
  }


  labels_y <- list(
    BB0 = expression(B/B[0]),
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY]),
    Bdev = "Process Error on log(Biomass)",
    B = "Biomass (t)",
    H = "Harvest rate",
    Catch = "Catch",
    BBfrac = expression(B/B[frac]),
    Bref = expression(B[REF])
  )

  if (is.null(title_y)) {
    title_y <- labels_y[[indicator_name]]
  }

  y_decimals <- ifelse(y_lim[2] > 10, 0, 1)

  y_labels <- if (use_si_suffix) {
    function(x) .international_system_prefixes(x)
  } else {
    function(x) {
      format(x, nsmall = y_decimals, scientific = FALSE, 
        big.mark = ".", decimal.mark = ",")
    }
  }

  p <- ggplot() +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2))
  
  if (indicator_name == "BBmsy") {
    p <- p +
      geom_hline(yintercept = 1, linetype = "longdash") +
      geom_hline(yintercept = 0.4, linetype = "longdash", colour = "red")
  }
  else if (indicator_name == "FFmsy") {
    p <- p +
      geom_hline(yintercept = 1, linetype = "longdash")
  }
  else if (indicator_name == "Bdev") {
    p <- p +
      geom_hline(yintercept = 0, linetype = "longdash")
  }
  
  p <- p +
    geom_line(data = df, aes(x = year, y = mu), linewidth = 1) +
    facet_wrap(~ Scenario, scales = "free_x", ncol = n_col) +
    scale_y_continuous(
      expand = c(0, 0), 
      labels = y_labels
    ) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
  p
}