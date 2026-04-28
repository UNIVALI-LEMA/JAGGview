#' Summarise trajectory data from model outputs
#'
#' This function computes summary statistics (median and quantiles)
#' for selected variables from model outputs, grouped by year and scenario.
#'
#' @param model_results A data.frame containing model outputs. Must include
#' columns \code{year}, \code{run}, and the variables used in the analysis
#' (e.g., \code{BB0}, \code{stock}, \code{harvest}), returned by the JABBA 
#' function \code{JABBA::jbplot_ensemble()}.
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
#'   \item \code{metric}: Name of the metric (indicator) summarised
#' }
#'
#' @details
#' The function maps user-friendly variable names to:
#' \itemize{
#'   \item \code{"BB0"}: \code{BB0}
#'   \item \code{"BBmsy"}: \code{stock}
#'   \item \code{"FFmsy"}: \code{harvest}
#' }
#'
#' @examples
#' \dontrun{
#' model_results <- jbplot_ensemble()
#' df <- trajectories_data(model_results)
#' df
#' }
#' 
#' @export
#' @importFrom dplyr %>% rename mutate summarise bind_rows ungroup
#' @importFrom stats median quantile
trajectories_data <- function(model_results) {
  ###@> Filtering the expected data...
  .validate_jbplot_ensemble(model_results)

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
        metric = var_name,
        .by = c(year, Scenario)
      )
  })

  bind_rows(result_list) %>%
    ungroup()
}

#' Plot model trajectories
#'
#' Creates a ggplot2-based visualization of model trajectories over time,
#' including median trends and uncertainty intervals across scenarios.
#'
#' @param df A data frame as returned by \code{trajectories_data()}.
#' @param variable A character string indicating the variable to plot.
#'   Options are \code{"BB0"}, \code{"BBmsy"}, \code{"FFmsy"}, \code{"Bdev"}, 
#'   \code{"B"}, \code{"H"}, \code{"Catch"}, \code{"BBfrac"} or \code{"Bref"}.
#' @param palette A character vector of colors used for plotting.
#'   Defaults to "#4285f4"
#' @param title_y A character string or expression for the y-axis label.
#'   Defaults to a predefined label depending on the selected variable.
#'
#' @return A ggplot object displaying trajectory summaries with
#'   confidence intervals, faceted by scenario.
#'
#' @details
#' The functions filters the input data based on the selected \code{variable}
#' (matching the \code{metric} column). The plot includes ribbons representing 
#' 80% (\code{lcl2}-\code{ucl2}) and 95% (\code{lcl}-\code{ucl}) confidence 
#' intervals, as well as a median trajectory line (\code{mu}).
#'
#' Reference lines are added depending on the selected variable:
#' \itemize{
#'   \item \code{"BBmsy"}: horizontal lines at 1 and 0.4
#'   \item \code{"FFmsy"}: horizontal line at 1
#'   \item \code{"Bdev"}: horizontal line at 0
#' }
#'
#' The y-axis limits are automatically adjusted based on the data range,
#' and labels are formatted dynamically. The y-axis label is automatically
#' defined unless provided by the user.
#' 
#' @examples
#' \dontrun{
#' df <- trajectories_data(out)
#' trajectories_ggplot(df, variable = "BB0", palette = c("blue"))
#' }
#'
#'
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line facet_wrap 
#' scale_y_continuous labs theme
trajectories_ggplot <- function(
  df, variable, palette = "#4285f4", title_y = NULL
) {
  if(!variable %in% c("BB0", "BBmsy", "FFmsy", "Bdev", "B", "H", "Catch", "BBfrac", "Bref")) {
    stop(
      "Parameter 'variable' was expecting 'BB0', 'BBmsy', 'FFmsy', 'Bdev', 'B', 'H', 'Catch', 
      'BBfrac' or 'Bref'."
    )
  }

  .is_palette_valid(palette)

  df <- df %>%
    filter(metric == variable)

  max_val <- .round_to_nearest(max(df$ucl, na.rm = TRUE), TRUE)
  min_val <- .round_to_nearest(min(df$lcl, na.rm = TRUE), FALSE)

  labels_y <- list(
    BB0 = expression(B/B[0]),
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY]),
    Bdev = "Process Error on log(Biomass)", # This and below are in development
    B = "Biomass (t)",
    H = "H",
    Catch = "Catch",
    BBfrac = expression(B/B[frac]),
    Bref = expression(B[REF])
  )

  if (is.null(title_y)) {
    title_y <- labels_y[[variable]]
  }

  y_decimals <- ifelse(max_val > 10, 0, 1)

  p <- ggplot() +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2))
  
  if (variable == "BBmsy") {
    p <- p +
      geom_hline(yintercept = 1, linetype = "longdash") +
      geom_hline(yintercept = 0.4, linetype = "longdash", colour = "red")
  }
  else if (variable == "FFmsy") {
    p <- p +
      geom_hline(yintercept = 1, linetype = "longdash")
  }
  else if (variable == "Bdev") {
    p <- p +
      geom_hline(yintercept = 0, linetype = "longdash")
  }
  
  p <- p +
    geom_line(data = df, aes(x = year, y = mu), linewidth = 1) +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(
      expand = c(0, 0), 
      limits = c(min_val, max_val),
      labels = function(x) .format_number(x, decimals = y_decimals)
    ) +
    labs(x = "Year", y = title_y) +
    .my_theme() +
    theme(legend.position = "none")
  p
}