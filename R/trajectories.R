#' Summarise trajectory data from model outputs
#'
#' Computes summary statistics (median and quantiles) selected variables from 
#' model outputs, grouped by year and scenario.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#'
#' @return A data.frame containing:
#' \itemize{
#'   \item \code{year}: Year of the observation
#'   \item \code{Scenario}: Scenario name
#'   \item \code{mu}: Median value
#'   \item \code{lcl}: Lower 2.5% quantile
#'   \item \code{ucl}: Upper 97.5% quantile
#'   \item \code{lcl2}: Lower 10% quantile
#'   \item \code{ucl2}: Upper 90% quantile
#'   \item \code{indicator}: Name of the indicator summarised
#' }
#'
#' @details
#' The function maps user-friendly indicator_name names to:
#' \itemize{
#'   \item \code{"BB0"}: \code{BB0}
#'   \item \code{"BBmsy"}: \code{stock}
#'   \item \code{"FFmsy"}: \code{harvest}
#' }
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- trajectories_data(list_fit_models)
#' df
#' }
#' 
#' @export
#' @importFrom dplyr %>% rename mutate summarise bind_rows ungroup
#' @importFrom stats median quantile
trajectories_data <- function(list_fit_models) {
  # ###@> Filtering the expected data...
  # .validate_jbplot_ensemble(model_results)
  model_results <- .jbplot_ensemble2(
    kb = list_fit_models,
    kbout = TRUE,
    plot = FALSE
  )

  columns <- list(
    BB0   = "BB0",
    BBmsy = "stock",
    FFmsy = "harvest",
    Bdev  = "Bdev",
    B = "B", # This and below are in development
    H = "H",
    Catch = "Catch",
    BBfrac = "BBfrac",
    Bref = "Bref"
  )

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

  result_list <- lapply(names(columns), function(var_name) {

    var_col <- columns[[var_name]]

    model_results %>%
      summarise(
        mu   = median(.data[[var_col]]),
        lcl  = quantile(.data[[var_col]], probs = 0.025),
        ucl  = quantile(.data[[var_col]], probs = 0.975),
        lcl2 = quantile(.data[[var_col]], probs = 0.1),
        ucl2 = quantile(.data[[var_col]], probs = 0.9),
        indicator = var_name,
        .by = c(year, Scenario)
      )
  })

  results <- bind_rows(result_list) %>%
    ungroup()

  class(results) <- c("JAGGdata", class(results))

  if (all(is.na(results))) {
    stop("Data frame only have NA data.")
  }

  return(results)
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
#'
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line facet_wrap 
#' scale_y_continuous labs theme
trajectories_ggplot <- function(
  df, indicator_name, n_col = 3, use_si_suffix = FALSE, palette = NULL, 
  title_y = NULL, x_lim = NULL, y_lim = NULL
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
    function(x) .format_number(x, decimals = y_decimals)
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
    labs(x = "Year", y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
  p
}