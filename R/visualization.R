#' Plot CPUE residuals diagnostics
#' 
#' Creates a ggplot2-based visualization of the CPUE residuals by year, index 
#' and scenarios, with a smoothed trend and the RMSE of each scenario. The 
#' input is the output of \code{\link{runs_tests_data}()}.
#' 
#' @details 
#' Each panel shows one scenario. Residuals are drawn as points joined by
#' segments to the \code{Ref} value, with a horizontal line at zero. The
#' smoothed trend (black line and band) is computed by
#' \code{ggplot2::geom_smooth()} on the residuals of the panel, and the RMSE
#' table is placed at the position given by \code{position}.
#' 
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{runs_tests_data}()}, with the elements
#'   \code{cpue_residuals} and \code{RMSE_data}.
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
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Residuals".
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param y_decimals Optional. Number of decimal places y-axis labels. 
#'   If \code{NULL}, up to two decimals places are shown and trailling zeros 
#'   are dropped. Defaults to NULL.
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL}, a color-blind-friendly palette is generated automatically 
#'   according to the number of index levels. If the number of supplied colors 
#'   is smaller then the number specified, then the code returns an error. 
#'   Defaults to NULL.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max). If \code{NULL}, the limits will be 
#'   calculated based on the data. Defaults to NULL. 
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max). If \code{NULL}, the limits will be 
#'   calculated based on the data. Defaults to NULL. 
#' 
#' @return A \code{ggplot} object.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' data <- runs_tests_data(list(fit.S01, fit.S02))
#' cpue_residuals_ggplot(data)
#'
#' # Table at the bottom right, in two columns
#' cpue_residuals_ggplot(data, n_col = 2, position = "bottom-right")
#' }
#' 
#' @family visualization functions
#' @family cpue residuals runs tests functions
#' 
#' @export
#' @importFrom ggplot2 .pt aes coord_cartesian facet_wrap geom_hline geom_point 
#' geom_segment geom_smooth ggplot labs scale_colour_manual scale_fill_manual 
#' scale_y_continuous theme
#' @importFrom grDevices colorRampPalette
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
cpue_residuals_ggplot <- function(
  df_lists, n_col = 3, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Residuals", use_si_suffix = FALSE, y_decimals = NULL, 
  palette = NULL, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  
  # One color per index
  n_levels <- length(unique(df_lists$cpue_residuals$Index))
  palette <- .resolve_palette(palette, n_levels)

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(
      max(df_lists$cpue_residuals$Res, na.rm = TRUE), TRUE
    )
    min_y_val <- .round_to_nearest(
      min(df_lists$cpue_residuals$Res, na.rm = TRUE), FALSE
    )
    y_lim <- c(min_y_val, max_y_val)
  }
  
  if (is.null(x_lim)) x_lim <- range(df_lists$cpue_residuals$Year, na.rm = TRUE)
  
  # RMSE table placed in each panel
  table <- .prepare_npc_table_data(
    data = df_lists$RMSE_data, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = Value, 
    col_name = "RMSE", 
    suffix = "%"
  )
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(
      data = df_lists$cpue_residuals, 
      aes(x = Year, xend = Year, y = Ref, yend = Res, colour = Index)
    ) +
    geom_point(
      data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res, fill = Index, colour = Index),
      pch = 21, size = 2
    ) +
    geom_smooth(
      data = df_lists$cpue_residuals, 
      aes(x = Year, y = Res), 
      se = TRUE, colour = "black"
    ) +
    geom_table_npc(
      data = table,
      aes(npcx = x, npcy = y, label = tb), 
      size = text_size, 
      table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = n_col) +
    scale_y_continuous(expand = c(0, 0), labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    labs(x = title_x, y = title_y, fill = "", colour = "") +
    .my_theme() +
    theme(legend.position = "top")
}

#' Plot fitted indices with credibility intervals
#' 
#' Creates a ggplot2-based visualization of the observed abundance indices 
#' together with the fitted values and their 80% and 95% credibility 
#' intervals, by scenario and index. The input is the output of 
#' \code{\link{fits_data}()}.
#' 
#' @details
#' Each panel shows one scenario (rows) and one index (columns). The ribbons
#' are the 80% and 95% credibility intervals of the fit, the line is the
#' fitted value, and the points with error bars are the observed values with
#' their bounds (\code{Li} and \code{Ui}).
#'
#' The axis limits (\code{x_lim} and \code{y_lim}) are applied with
#' \code{ggplot2::coord_cartesian()}, so they are the same in all panels.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{fits_data}()}, with the elements \code{Li_Ui}, \code{CI_80}
#'   and \code{CI_95}.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Abundance index".
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param y_decimals Optional. Number of decimal places in the y-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param palette Optional. A character vector of colors used for plotting.
#'   Only the first color is used (the fill of the credibility intervals).
#'   If \code{NULL}, a default color-blind-friendly color is used. An error
#'   is returned if the supplied vector is invalid. Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits are calculated from the years in the data. Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits are calculated from the 95% credibility intervals, rounded.
#'   Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' data <- fits_data(list(fit.S01, fit.S02))
#' fits_ggplot(data)
#'
#' # Custom color and y-axis limits
#' fits_ggplot(data, palette = "#1B4F8A", y_lim = c(0, 5))
#' }
#' 
#' @family visualization functions
#' @family fits functions
#'
#' @export
#' @importFrom ggplot2 aes coord_cartesian facet_grid geom_errorbar geom_line 
#' geom_point geom_ribbon ggplot labs scale_y_continuous
fits_ggplot <- function(
  df_lists, title_x = "Year", title_y = "Abundance index", 
  use_si_suffix = FALSE, y_decimals = NULL, palette = NULL, x_lim = NULL, 
  y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  # A single color is needed
  palette <- .resolve_palette(palette, 1)

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$CI_95$uci, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(df_lists$CI_95$lci, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) x_lim <- range(df_lists$CI_80$Year, na.rm = TRUE) 
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }

  ggplot() +
    geom_ribbon(
      data = df_lists$CI_80,
      aes(x = Year, ymin = lci, ymax = uci),
      alpha = 0.3, fill = palette[1]
    ) +
    geom_ribbon(
      data = df_lists$CI_95,
      aes(x = Year, ymin = lci, ymax = uci),
      alpha = 0.3, fill = palette[1]
    ) +
    geom_line(data = df_lists$CI_80, aes(x = Year, y = mu)) +
    geom_errorbar(
      data = df_lists$Li_Ui,
      aes(x = Year, ymin = Li, ymax = Ui), 
      width = 1.5
    ) +
    geom_point(
      data = df_lists$Li_Ui,
      aes(x = Year, y = Mean),
      pch = 21, fill = "white", size = 1.5
    ) + 
    facet_grid(Scenario ~ Index, scales = "free") +
    scale_y_continuous(labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) + 
    labs(x = title_x, y = title_y) +
    .my_theme()
}

#' Plot hindcast diagnostics
#'
#' Creates a ggplot2-based visualization of the hindcast (retrospective
#' forecast) diagnostics, showing the observed and predicted values, the
#' credibility intervals of the reference run and the MASE of each index, by
#' scenario and index. The input is the output of \code{\link{hindcast_data}()}.
#' 
#' @details
#' Each panel shows one scenario (rows) and one index (columns). The gray
#' ribbon is the credibility interval of the reference run, the colored lines
#' are the retrospective runs, and the white lines are the hindcast
#' trajectories. Points show the observed values (large) and the predicted
#' values (small) at the first hindcast year of each run. The MASE of each 
#' panel is shown in a table.
#' 
#' The colors of the retrospective runs come from \code{JABBA::ss3col()},
#' which supports up to 8 runs.
#' 
#' If \code{zoom = TRUE}, a zoomed view of the hindcast window is inserted in
#' the upper right corner of each panel, and the window is marked by a dashed
#' rectangle. Panels with no data for a Scenario and Index combination are left 
#' blank.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{hindcast_data}()}, with the elements \code{data}, 
#'   \code{data_points}, \code{data_lines}, \code{mase_data} and
#'   \code{min_year_retro}.
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
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param zoom A boolean value that if \code{TRUE} adds a zoomed view of the 
#'   hindcast window to each panel. Defaults to \code{FALSE}.
#' @param y_decimals Optional. Number of decimal places in the y-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the years in the data. Defaults to
#'   \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the credibility intervals of the
#'   fitted values, rounded. Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' The plot includes credibility ribbons for reference runs, hindcast
#' trajectories, observed and predicted points, and annotations of MASE values. 
#' Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # Prepare the data and plot
#' data <- hindcast_data(list(hc_S01, hc_S02))
#' hindcast_ggplot(data)
#'
#' # With the zoomed view and the MASE table at the bottom right
#' hindcast_ggplot(data, zoom = TRUE, position = "bottom-right")
#' }
#' 
#' @family visualization functions
#' @family hindcasts functions
#'
#' @export 
#' @importFrom ggplot2 .pt aes coord_cartesian facet_grid geom_line geom_point 
#' geom_rect geom_ribbon ggplot guide_legend guides labs scale_colour_manual 
#' scale_fill_manual scale_y_continuous theme 
#' @importFrom dplyr %>% filter mutate select vars
#' @importFrom JABBA ss3col
#' @importFrom ggpp geom_plot geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
hindcast_ggplot <- function(
  df_lists, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Index", use_si_suffix = FALSE, zoom = FALSE, y_decimals = NULL, 
  x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(
      max(df_lists$data$hat.uci, na.rm = TRUE), TRUE
    )
    min_y_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), 
    FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    x_lim <- range(df_lists$data$year)
  }
  
  # MASE table placed in each panel
  table <- .prepare_npc_table_data(
    data = df_lists$mase_data, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = MASE, 
    col_name = "MASE", 
    decimals = 3
  )

  # Hindcast window
  min_year_hc <- min(df_lists$data_lines$year) - 1
  max_year_hc <- max(df_lists$data_lines$year)
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }
  
  p1 <- ggplot() +
    geom_ribbon(
      data = filter(df_lists$data, retro.peels == 0),
      aes(x = year, ymin = hat.lci, ymax = hat.uci),
      fill = "gray80"
    ) +
    geom_ribbon(
      data = filter(
        df_lists$data, retro.peels == 0, year < df_lists$min_year_retro
      ),
      aes(x = year, ymin = hat.lci, ymax = hat.uci),
      fill = "gray30", alpha = 0.5
    ) +
    geom_line(
      data = filter(df_lists$data, hindcast == FALSE),
      aes(x = year, y = hat, colour = retro), 
      linewidth = 1
    ) +
    geom_line(
      data = df_lists$data_lines,
      aes(x = year, y = hat, group = retro.peels),
      linewidth = 1, colour = "white"
    ) +
    geom_point(
      data = filter(
        df_lists$data, retro.peels == 0, year < df_lists$min_year_retro
      ),
      aes(x = year, y = obs), 
      pch = 21, size = 4, fill = "white"
    ) +
    geom_point(
      data = df_lists$data_points, 
      aes(x = year, y = obs, fill = retro),
      show.legend = FALSE, pch = 21, size = 4
    ) +
    geom_point(
      data = df_lists$data_points,
      aes(x = year, y = hat, fill = retro),
      show.legend = FALSE,pch = 21, size = 2
    ) +
    geom_table_npc(
      data = table,
      aes(npcx = x, npcy = y, label = tb),
      size = text_size,
      table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
    labs(x = title_x, y = title_y, colour = "") +
    scale_y_continuous(labels = y_labels) +
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

  # Zoomed view of the hindcast window, one per Scenario and Index
  combos <- expand.grid(
    Scenario = unique(df_lists$data$Scenario),
    Index = unique(df_lists$data$Index),
    stringsAsFactors = FALSE
  )

  combos$plot <- Map(
    .make_zoom_plot, 
    combos$Scenario, 
    combos$Index,
    MoreArgs = list(
      df = df_lists,
      min_year = min_year_hc,
      max_year = max_year_hc,
      y_labels = y_labels
    )
  )

  # Keep only the panels that have data
  combos_valid <- combos %>% 
    filter(!sapply(plot, is.null))
  combos_valid$x <- x_lim[2]
  combos_valid$y <- y_lim[2]

  # y range of the zoom window
  zoom_ribbon_all <- df_lists$data %>%
    filter(year >= min_year_hc, retro.peels == 0)

  min_y_zoom <- min(
    floor(min(zoom_ribbon_all$hat.lci) * 10) / 10,
    floor(min(df_lists$data_points$obs) * 10) / 10,
    floor(min(df_lists$data_lines$obs) * 10) / 10
  )
  max_y_zoom <- max(
    ceiling(max(zoom_ribbon_all$hat.uci) * 10) / 10,
    ceiling(max(df_lists$data_points$obs) * 10) / 10,
    ceiling(max(df_lists$data_lines$obs) * 10) / 10
  )

  # Dashed rectangle marking the zoom window
  rect_data <- combos_valid %>%
    select(Scenario, Index) %>%
    mutate(
      xmin = min_year_hc, xmax = max_year_hc, 
      ymin = min_y_zoom,  ymax = max_y_zoom
    )

  p1 +
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
}

#' Plot Kobe diagram
#' 
#' Creates a ggplot2-based visualization of the stock status in a Kobe plot,
#' with biomass (B/Bmsy) on the x-axis and fishing mortality (F/Fmsy) on the
#' y-axis, showing the credibility contours of the terminal year and the
#' trajectory of each scenario. The input is the output of
#' \code{\link{kobe_data}()}.
#'
#' @details
#' Each panel shows one scenario. The background is divided into four
#' quadrants by the reference lines at B/Bmsy = 1 and F/Fmsy = 1:
#' \itemize{
#'   \item green: B/Bmsy > 1 and F/Fmsy < 1;
#'   \item yellow: B/Bmsy < 1 and F/Fmsy < 1;
#'   \item orange: B/Bmsy > 1 and F/Fmsy > 1;
#'   \item red: B/Bmsy < 1 and F/Fmsy > 1.
#' }
#'
#' The gray polygons are the kernel density contours of the terminal year,
#' one for each credibility level chosen in \code{kobe_data()} (argument
#' \code{ci_levels}). The line is the trajectory of the median B/Bmsy and
#' F/Fmsy, and the points mark the first, middle and last years of the series
#' (circle, square and diamond, respectively).
#'
#' The quadrants extend up to the maximum ratio found in the data, so axis
#' limits larger than that show blank areas. The lower limits cannot be
#' negative, and the axis breaks are placed at every 1 unit.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{kobe_data}()}, with the elements \code{col01}, \code{col02},
#'   \code{col03}, \code{col04}, \code{ci_data}, \code{data_lines} and
#'   \code{highlight_years}.
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param title_x A character string or an expression for the x-axis label. 
#'   Defaults to \code{expression(B/B[MSY])}.
#' @param title_y A character string or an expression for the y-axis label.
#'   Defaults to \code{expression(F/F[MSY])}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from 0 to the maximum
#'   B/Bmsy. Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from 0 to the maximum
#'   F/Fmsy. Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' data <- kobe_data(list(fit.S01, fit.S02))
#' kobe_ggplot(data)
#'
#' # In two columns, with custom axis limits
#' kobe_ggplot(data, n_col = 2, x_lim = c(0, 3), y_lim = c(0, 2))
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

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y <- df_lists$col02$ymax
    y_lim <- c(0, max_y)
  }
  if (is.null(x_lim)) {
    max_x <- df_lists$col02$xmax
    x_lim <- c(0, max_x)
  }

  # One color per index
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
    facet_wrap(~ Scenario, scales = "fixed", ncol = n_col) +
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
#' Creates a ggplot2-based visualization comparing the prior and posterior
#' densities of a selected parameter (K, r or psi), by scenario, annotated with
#' the prior-posterior mean and variance ratios. The input is the output of
#' \code{\link{priors_posteriors_data}()}.
#'
#' @details
#' Each panel shows one scenario. The prior density is drawn with the first
#' color of \code{palette} and the posterior density with the second one. The
#' table in each panel shows the prior-posterior mean ratio (PPMR) and the
#' prior-posterior variance ratio (PPVR) of the selected parameter.
#'
#' The y-axis labels and ticks are hidden, since densities are only compared
#' in shape. The axis limits are applied with
#' \code{ggplot2::coord_cartesian()}, so they are the same in all panels.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{priors_posteriors_data}()}, with the elements \code{prior},
#'   \code{posterior}, \code{PPVR} and \code{PPMR}.
#' @param indicator_name A character string specifying the parameter to plot.
#'   Must be one of \code{"K"}, \code{"r"} or \code{"psi"}.
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
#'   \code{"bottom-center"}, and \code{"bottom-right"}. Defaults to
#'   \code{"top-left"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Density".
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param palette Optional. A character vector of colors used for plotting,
#'   with at least two colors (prior and posterior). If \code{NULL}, a
#'   color-blind-friendly palette with two colors is generated automatically.
#'   If more than two colors are supplied, only the first two are used. If the
#'   number of supplied colors is smaller than two, then the code returns an
#'   error. Defaults to \code{NULL}.
#' @param title_x Optional. A character string for the x-axis label. If
#'   \code{NULL}, a default label is assigned based on \code{indicator_name}
#'   ("Carrying capacity (K)", "Intrinsic growth rate (r)" or "Initial biomass
#'   depletion ratio (psi)"). Defaults to \code{NULL}.
#' @param x_decimals Optional. Number of decimal places in the x-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from the minimum to the
#'   maximum value of the prior and posterior. For \code{"K"}, the upper
#'   limit is the 95% quantile of these values, to avoid the long tail.
#'   Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from the minimum to the
#'   maximum density of the prior and posterior, rounded. Defaults to
#'   \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' data <- priors_posteriors_data(list(fit.S01, fit.S02))
#' priors_posteriors_ggplot(data, "K")
#'
#' # Custom colors and SI suffixes on the x-axis
#' priors_posteriors_ggplot(
#'   data, "K", use_si_suffix = TRUE, palette = c("#4285f4", "#34a853")
#' )
#' }
#' 
#' @family visualization functions
#' @family priors vs posteriors functions
#'
#' @export
#' @importFrom dplyr %>% all_of full_join pull rename select
#' @importFrom ggplot2 .pt aes coord_cartesian element_blank facet_wrap 
#' geom_area ggplot labs scale_x_continuous scale_y_continuous theme
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
#' @importFrom stats quantile
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

  # Colors for prior and poseterior
  palette <- .resolve_palette(palette, 2)

  # Validate the axis index
  .axis_limit(x_lim)
  .axis_limit(y_lim)

  # Select the prior data from the indicator
  indicator1 <- paste0(indicator_name, "01")
  indicator2 <- paste0(indicator_name, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(value_1 = all_of(indicator1), value_2 = all_of(indicator2))
  
  # Title set to receive label based on indicator
  labels_x <- list(
    K = "Carrying capacity (K)",
    r = "Intrinsic growth rate (r)",
    psi = "Initial biomass depletion ratio (psi)"
  )
  if (is.null(title_x)) title_x <- labels_x[[indicator_name]]
  
  # Select the posterior data from the indicator
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(value_1 = all_of(indicator1), value_2 = all_of(indicator2))

  # Default limits from the data
  if (is.null(x_lim)) {
    x_min <- min(prior$value_1, posterior$value_1, na.rm = TRUE)
    x_max <- ifelse(
      indicator_name != "K", 
      max(prior$value_1, posterior$value_1, na.rm = TRUE),
      quantile(c(prior$value_1, posterior$value_1), 0.95, na.rm = TRUE)
    )
    x_lim <- c(ifelse(indicator_name != "K", x_min, x_min - 1), x_max)
  }
  if (is.null(y_lim)) {
    max_y <- .round_to_nearest(
      max(prior$value_2, posterior$value_2, na.rm = TRUE), TRUE, 1.1
    )
    min_y <- .round_to_nearest(
      min(prior$value_2, posterior$value_2, na.rm = TRUE), FALSE, 1.1
    )
    y_lim <- c(min_y, max_y)
  }
  
  df_text <- df_lists$PPMR %>%
  select(Scenario, ppmr_value = all_of(indicator_name)) %>%
  full_join(
    df_lists$PPVR %>% 
      select(Scenario, ppvr_value = all_of(indicator_name)),
    by = "Scenario"
  ) 

  # PPMR and PPVR table placed in each panel
  table <- .prepare_npc_table_data(
    data = df_text,
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = c(ppmr_value, ppvr_value), 
    col_name = c("PPMR", "PPVR"),
    decimals = 3
  )

  x_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = x_decimals
    )
  }
  
  ggplot() +
    geom_area(
      data = prior, 
      aes(x = value_1, y = value_2),
      fill = palette[1], alpha = 0.5, colour = "black"
    ) +
    geom_area(
      data = posterior, 
      aes(x = value_1, y = value_2),
      fill = palette[2], alpha = 0.5, colour = "black"
    ) +
    geom_table_npc(
      data = table,
      aes(npcx = x, npcy = y, label = tb),
      size = text_size, 
      table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
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
#' Creates a ggplot2-based visualization of the retrospective analysis of a
#' selected indicator (B, F, B/Bmsy, F/Fmsy, process error or surplus 
#' production), by scenario, annotated with the retrospective bias (rho). The
#' input is the output of \code{\link{retrospective_analysis_data}()}.
#'
#' @details
#' Each panel shows one scenario. For the time series indicators (\code{"B"},
#' \code{"F"}, \code{"BBmsy"}, \code{"FFmsy"} and \code{"procB"}), the gray
#' ribbon is the credibility interval of the reference run, and the lines are
#' the reference run (black) and the retrospective runs (colored), drawn only
#' for the years flagged by the \code{keep} column. A dashed line marks 1 for
#' \code{"BBmsy"} and \code{"FFmsy"}, and 0 for \code{"procB"}.
#'
#' For \code{"MSY"}, the plot shows the surplus production curves as a function 
#' of biomass, one for each run.
#'
#' The table in each panel shows the retrospective bias (rho) of the selected
#' indicator. The colors of the retrospective runs come from
#' \code{JABBA::ss3col()}, which supports up to 8 runs. The axis limits are
#' applied with \code{ggplot2::coord_cartesian()}, so they are the same in all
#' panels.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{retrospective_analysis_data}()}, with the elements
#'   \code{data}, \code{surplus_data} and \code{rho_data}.
#' @param indicator_name A character string specifying the indicator to plot.
#'   Must be one of \code{"B"}, \code{"F"}, \code{"BBmsy"}, \code{"FFmsy"},
#'   \code{"procB"} or \code{"MSY"}.
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
#'   \code{"bottom-center"}, and \code{"bottom-right"}. Defaults to
#'   \code{"top-left"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param title_x Optional. A character string for the x-axis label. If
#'   \code{NULL}, a default label is assigned based on \code{indicator_name}
#'   ("Biomass (t)" for \code{"MSY"} and "Year" for the others). Defaults to
#'   \code{NULL}.
#' @param title_y Optional. A character string or an expression for the
#'   y-axis label. If \code{NULL}, a default label is assigned based on
#'   \code{indicator_name} (for example, "Biomass (t)" for \code{"B"} and
#'   "Surplus Production (t)" for \code{"MSY"}). Defaults to \code{NULL}.
#' @param x_decimals Optional. Number of decimal places in the x-axis labels,
#'   used only when \code{indicator_name} is \code{"MSY"}. If \code{NULL}, up
#'   to two decimal places are shown and trailing zeros are dropped. Defaults
#'   to \code{NULL}.
#' @param y_decimals Optional. Number of decimal places in the y-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data. Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data (the credibility interval of
#'   the reference run, or its surplus production for \code{"MSY"}), rounded.
#'   Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # Prepare the data and plot
#' data <- retrospective_analysis_data(list(hc_S01, hc_S02))
#' retrospective_analysis_ggplot(data, indicator_name = "B")
#'
#' # Surplus production curves, in two columns
#' retrospective_analysis_ggplot(data, "MSY", n_col = 2)
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
  use_si_suffix = FALSE, title_x = NULL, title_y = NULL, x_decimals = NULL, 
  y_decimals = NULL, x_lim = NULL, y_lim = NULL
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

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  if (indicator_name != "MSY") {
    data <- df_lists$data
    if (is.null(title_x)) title_x <- "Year"
  } else {
    data <- df_lists$surplus_data
    if (is.null(title_x)) title_x <- "Biomass (t)"
  }

  # Title set to receive label based on indicator
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
  
  # Filtering the data base on the indicator
  rho_data <- df_lists$rho_data
  data_var <- data %>% 
    filter(Index == indicator_name)
  data_ref <- data_var %>% 
    filter(id == "Ref")

  # Default limits from the data
  if (indicator_name != "MSY") {
    data_lines <- data_var %>% 
      filter(keep == TRUE)
  } else {
    data_lines <- data_var
  }
  rho_var <- rho_data %>% 
    filter(Index == indicator_name)
  if (indicator_name != "MSY") {
    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }
    if (is.null(x_lim)) x_lim <- range(data_ref$Year)
  } else {
    max_y_val <- .round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }
    if (is.null(x_lim)) x_lim <- range(data_ref$SB_i)
    max_x_val <- .round_to_nearest(max(data_ref$SB_i, na.rm = TRUE), TRUE, 1.1)
  }
  
  x_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = x_decimals
    )
  }
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }
  
  # rho table placed in each panel
  table <- .prepare_npc_table_data(
    data = rho_var, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = rho, 
    col_name = "\u03c1", 
    decimals = 3
  )
  
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
    data_lines <- data_lines %>% 
      filter(!is.na(data_lines$SB_i) & !is.na(data_lines$SP))

    p <- p +
      geom_line(
        data = data_lines,
        aes(x = SB_i, y = SP, colour = id, group = id),
        linewidth = 1
      )
  }
  
  p <- p +
    geom_table_npc(
      data = table,
      aes(npcx = x, npcy = y, label = tb), 
      size = text_size,
      table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
    facet_wrap(~Scenario, ncol = n_col, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    coord_cartesian(xlim = x_lim, ylim = y_lim)

  if (indicator_name == "MSY") {
    p <- p +
      scale_x_continuous(labels = x_labels)
  }
  
  p <- p +
    scale_y_continuous(expand = c(0, 0), labels = y_labels) +
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
#' Creates a ggplot2-based visualization of the runs test diagnostics of the 
#' CPUE residuals, showing the residuals, the 3-sigma limits of the test and 
#' it's p-value, by scenario and index. The input is the output of 
#' \code{\link{runs_tests_data}()}. 
#' 
#' @details
#' Each panel show one scenario (rows) and one index (columns). The shaded 
#' rectangle marks the 3-sigma limits of the runs test (\code{lcl} and
#' \code{ucl}) over the years of the series. It is green when the runs test
#' does not reject randomness (p-value >= 0.05) and red otherwise 
#' (p-value < 0.05). The residuals are drawn as points joined by segments to
#' the \code{Ref} value, with a horizontal line at zero. Residuals inside the
#' limits are white and residuals outside the limits are red and larger. The
#' p-value of each panel is shown in a table.
#'
#' @param df_lists A \code{JAGGdata} list as returned by 
#'   \code{\link{runs_tests_data}()}, with the elements \code{cpue_residuals}
#'   and \code{SE3}.
#' @param position A character string specifying the table's position within
#'   each plot panel, combining a vertical and a horizontal keyword separated
#'   by a hyphen, in the form \code{"<vertical>-<horizontal>"}. The vertical
#'   component must be one of \code{"top"}, \code{"middle"}, or
#'   \code{"bottom"}; the horizontal component must be one of \code{"left"},
#'   \code{"center"}, or \code{"right"}. Valid values are: \code{"top-left"},
#'   \code{"top-center"}, \code{"top-right"}, \code{"middle-left"},
#'   \code{"middle-center"}, \code{"middle-right"}, \code{"bottom-left"},
#'   \code{"bottom-center"}, and \code{"bottom-right"}. Defaults to
#'   \code{"top-left"}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 6.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to 
#'   "Residuals".
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param y_decimals Optional. Number of decimal places in the y-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from the first to the last
#'   year of the series. Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from the lowest to the
#'   highest 3-sigma limit, rounded. Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' df <- runs_tests_data(list(fit.S01, fit.S02))
#' runs_tests_ggplot(df)
#'
#' # With the p-value table at the bottom right
#' runs_tests_ggplot(df, position = "bottom-right")
#' }
#' 
#' @family visualization functions
#' @family cpue residuals runs tests functions
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 .pt aes coord_cartesian facet_grid geom_hline geom_point 
#' geom_rect geom_segment ggplot labs scale_fill_manual scale_y_continuous theme
#' @importFrom ggpp geom_table_npc ttheme_gtdefault
#' @importFrom stringr str_split_i
runs_tests_ggplot <- function(
  df_lists, position = "top-left", text_size = 6, title_x = "Year", 
  title_y = "Residuals", use_si_suffix = FALSE, y_decimals = NULL, 
  x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$SE3$ucl, na.rm = TRUE), TRUE, 
    2.5)
    min_y_val <- .round_to_nearest(min(df_lists$SE3$lcl, na.rm = TRUE), FALSE, 
    2.5)
    y_lim <- c(min_y_val, max_y_val)
  }
  if (is.null(x_lim)) x_lim <- range(df_lists$SE3$ymin, df_lists$SE3$ymax)

  # p-value table placed in each panel
  table <- .prepare_npc_table_data(
    data = df_lists$SE3, 
    pos_x = str_split_i(position, "-", 2), 
    pos_y = str_split_i(position, "-", 1), 
    col = pvalue, 
    col_name = "p-value", 
    decimals = 3
  )
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }

  ggplot() +
    geom_rect(
      data = df_lists$SE3,
      aes(xmin = ymin, xmax = ymax, ymin = lcl, ymax = ucl, fill = class),
      alpha = 0.2
    ) +
    geom_table_npc(
      data = table,
      aes(npcx = x, npcy = y,label = tb), 
      size = text_size,
      table.theme = ttheme_gtdefault(base_size = text_size * .pt)
    ) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_segment(
      data = df_lists$cpue_residuals,
      aes(x = Year, xend = Year, y = Ref, yend = Res)
    ) +
    geom_point(
      data = filter(df_lists$cpue_residuals, class == "white"),
      aes(x = Year, y = Res), 
      fill = "white", pch = 21, size = 2
    ) +
    geom_point(
      data = filter(df_lists$cpue_residuals, class == "red"), 
      aes(x = Year, y = Res), 
      fill = "red", pch = 21, size = 2.5
    ) +
    facet_grid(Scenario ~ Index, scales = "fixed") +
    scale_fill_manual(values = c("green", "red")) +
    scale_y_continuous(labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
}

#' Create and display a summary table
#' 
#' Creates a formatted table, using the \pkg{gt} package, from the data frames
#' returned by the package \code{get_()} extraction functions. The table can be 
#' displayed in the Viewer pane or printed to the console, and optionally saved 
#' to file.
#' 
#' @details
#' The table has bold column labels and alternating row background colors.
#'
#' When \code{show} is \code{"html"} or \code{"png"}, the table is written to
#' a temporary file and opened in the RStudio Viewer, or in the default web
#' browser if the Viewer is not available. Nothing is displayed in
#' non-interactive sessions. If \code{show = "console"}, the raw input data
#' frame is printed instead of the formatted table.
#'
#' If \code{save = "xlsx"}, the raw input data frame is exported with
#' \pkg{openxlsx}. For the other formats, the formatted table is exported
#' with \code{gt::gtsave()}. The file is saved in the working directory,
#' unless \code{filename} includes a path. Saving as \code{"png"} or
#' \code{"pdf"} requires the \pkg{webshot2} package.
#' 
#' @param data A data frame with the extracted model results, as returned by 
#'   one of the package \code{get_*()} functions.
#' @param show Optional. Character string specifying how the table should be
#'   displayed. Must be one of:
#'   \itemize{
#'     \item \code{"html"}: displays the table as an HTML file in the Viewer.
#'     \item \code{"png"}: displays the table as a PNG image in the Viewer.
#'     \item \code{"console"}: prints the raw data frame to the console.
#'     \item \code{NULL}: nothing is displayed.
#'   }
#'   Defaults to \code{"html"}.
#' @param save Optional. Character string specifying the format used to save 
#'   the table. Must be one of:
#'   \itemize{
#'     \item \code{"html"}: saves the table as an HTML file.
#'     \item \code{"png"}: saves the table as a PNG image.
#'     \item \code{"pdf"}: saves the table as a PDF file.
#'     \item \code{"xlsx"}: saves the raw data as an Excel file.
#'     \item \code{NULL}: no file is saved.
#'   }
#'   Defaults to \code{NULL}.
#' @param dir Optional. Character string for the output directory name, creates 
#'   it if don't exists. Defaults to \code{NULL}.
#' @param filename A character string for the output file name, without the 
#'   extension. Defaults to \code{"summary_table"}.
#' @param digits An integer indicating the number of decimals places to display. 
#'   Defaults to 4.
#' 
#' @examples
#' \dontrun{
#' # Table in the Viewer
#' summary_table(get_mase(hindcast_data(list_hc_models)))
#'
#' # PNG image in the Viewer
#' summary_table(get_ppmr(priors_posteriors_data(list_fit_models)), "png")
#'
#' # Print to the console and save as an Excel file in the results folder
#' summary_table(
#'   data = get_pars(list_fit_models), show = "console", save = "xlsx", 
#'   dir = "results", filename = "pars_table"
#' )
#' }
#' 
#' @family visualization functions
#' 
#' @export
#' @importFrom gt gtsave
#' @importFrom openxlsx write.xlsx
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom utils browseURL
summary_table <- function(
  data, show = "html", save = NULL, dir = NULL, filename = "summary_table", 
  digits = 4
) {
  table <- .default_table(data, digits = digits)
  if (is.null(table)) stop("Argument 'data' cannot be NULL or empty.")
  
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

    if (!is.null(dir)) {
      if (!dir.exists(dir)) {
        dir.create(dir)
      }
      file <- file.path(dir, paste0(filename, ext))
    }
    else {
      file <- paste0(filename, ext)
    }

    
    if (ext != ".xlsx") {
      gtsave(table, file)
    }
    else {
      write.xlsx(data, file)
    }
    message("Table saved to: ", normalizePath(file))
  }
  invisible(table)
}

#' Plot model trajectories
#'
#' Creates a ggplot2-based visualization of the trajectories of a selected 
#' indicator over time (including median trends and uncertainty intervals), by 
#' scenarios. The input is the output of \code{\link{trajectories_data}()}.
#' 
#' @details
#' The input data is filtered by the selected \code{indicator_name}, which 
#' matches the \code{indicator} column. Each panel shows one scenario. The 
#' light ribbons are the 80% (\code{lcl2}-\code{ucl2}) and 95%
#' (\code{lcl}-\code{ucl}) credibility intervals, and the line is the median
#' trajectory (\code{mu}). Both ribbons use the first color of
#' \code{palette}.
#'
#' Reference lines are added depending on the selected indicator:
#' \itemize{
#'   \item \code{"BBmsy"}: dashed lines at 1 and at \code{blim} (red);
#'   \item \code{"FFmsy"}: dashed line at 1;
#'   \item \code{"Bdev"}: dashed line at 0.
#' }
#'
#' @param df A \code{JAGGdata} data frame as returned by
#'   \code{\link{trajectories_data}()}.
#' @param indicator_name A character string indicating the indicator to plot. 
#'   Must be one of \code{"BB0"}, \code{"BBmsy"}, \code{"FFmsy"}, 
#'   \code{"Bdev"}, \code{"B"}, \code{"H"} or \code{"Catch"}.
#' @param blim A numeric value indicating the limit reference point,
#'   displayed as a red dashed horizontal line. Only used when
#'   \code{indicator_name} is \code{"BBmsy"}. Defaults to \code{0.4}.
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param y_decimals Optional. Number of decimal places in the y-axis labels.
#'   If \code{NULL}, up to two decimal places are shown and trailing zeros
#'   are dropped. Defaults to \code{NULL}.
#' @param palette Optional. A character vector of colors used for plotting.
#'   Only the first color is used (the fill of the credibility intervals). If
#'   \code{NULL}, a color-blind-friendly color is generated automatically.
#'   If the vector is invalid, then the code returns an error. Defaults to
#'   \code{NULL}.
#' @param title_y Optional. A character string or an expression for the
#'   y-axis label. If \code{NULL}, a default label is assigned based on
#'   \code{indicator_name} (for example, "Biomass (t)" for \code{"B"}).
#'   Defaults to \code{NULL}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the x-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the years of the selected indicator.
#'   Defaults to \code{NULL}.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower
#'   and upper limits of the y-axis, \code{c(min, max)}. If \code{NULL}, the
#'   limits will be calculated based on the data, from the lowest 2.5%
#'   quantile to the highest 97.5% quantile, rounded. Defaults to
#'   \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and plot
#' df <- trajectories_data(list(fit.S01, fit.S02))
#' trajectories_ggplot(df, indicator_name = "BBmsy")
#'
#' # Custom color and limit reference point
#' trajectories_ggplot(
#'   df, "BBmsy", blim = 0.5, palette = "#1B4F8A"
#' )
#' }
#' 
#' @family visualization functions
#' @family trajectories functions
#'
#' @export
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 aes coord_cartesian facet_wrap geom_hline geom_line 
#' geom_ribbon ggplot labs scale_y_continuous theme
trajectories_ggplot <- function(
  df, indicator_name, blim = 0.4, n_col = 3, title_x = "Year", use_si_suffix = FALSE, 
  y_decimals = NULL, palette = NULL, title_y = NULL, x_lim = NULL, 
  y_lim = NULL
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

  # A single color is needed
  palette <- .resolve_palette(palette, 1)

  # Validate the axis limits
  .axis_limit(y_lim)
  .axis_limit(x_lim)

  # Filter data based on the indicator
  df <- df %>%
    filter(indicator == indicator_name)

  # Default limits from the data
  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE, 1.1)
    min_y_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE, 1.1)
    y_lim <- c(min_y_val, max_y_val)
  }
  if (is.null(x_lim)) x_lim <- range(df$year, na.rm = TRUE)

  # Title set to receive label based on indicator
  labels_y <- list(
    BB0 = expression(B/B[0]),
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY]),
    Bdev = "Process Error on log(Biomass)",
    B = "Biomass (t)",
    H = "Harvest rate",
    Catch = "Catch"
  )
  if (is.null(title_y)) {
    title_y <- labels_y[[indicator_name]]
  }
  
  y_labels <- function(x) {
    .international_system_prefixes(
      number = x, use_si_suffix = use_si_suffix, decimals = y_decimals
    )
  }

  p <- ggplot() +
    geom_ribbon(
      data = df,
      aes(x = year, ymin = lcl, ymax = ucl), 
      fill = palette[1], alpha = 0.3
    ) +
    geom_ribbon(
      data = df,
      aes(x = year, ymin = lcl2, ymax = ucl2), 
      fill = palette[1], alpha = 0.3
    )
  
  if (indicator_name == "BBmsy") {
    p <- p +
      geom_hline(yintercept = blim, linetype = "longdash", colour = "red")
  }

  if (indicator_name %in% c("BBmsy", "FFmsy")) {
    p <- p +
      geom_hline(yintercept = 1, linetype = "longdash")
  }
  else if (indicator_name == "Bdev") {
    p <- p +
      geom_hline(yintercept = 0, linetype = "longdash")
  }
  
  p <- p +
    geom_line(data = df, aes(x = year, y = mu), linewidth = 1) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = n_col) +
    scale_y_continuous(expand = c(0, 0), labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
  p
}