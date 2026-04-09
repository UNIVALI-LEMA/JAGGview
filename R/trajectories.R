#' Summarise trajectory data from model outputs
#'
#' This function computes summary statistics (median and quantiles)
#' for selected variables from model outputs, grouped by year and scenario.
#'
#' @param list_models A data.frame containing model outputs. Must include
#' columns \code{year}, \code{run}, and the variables used in the analysis
#' (e.g., \code{BB0}, \code{stock}, \code{harvest}), returned by the JABBA 
#' function \code{JABBA::jbplot_ensemble()}.
#' @param variable A character string indicating which variable to summarise.
#' Options are \code{"BB0"}, \code{"BBmsy"}, or \code{"FFmsy"}.
#'
#' @return A data.frame containing:
#' \itemize{
#'   \item \code{year}: Year of the observation
#'   \item \code{Scenario}: Scenario name
#'   \item \code{mu}: Median value
#'   \item \code{lcl}: Lower 2.5\\% quantile
#'   \item \code{ucl}: Upper 97.5\\% quantile
#'   \item \code{lcl2}: Lower 10\\% quantile
#'   \item \code{ucl2}: Upper 90\\% quantile
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
#' list_models <- jbplot_ensemble()
#' df <- trajectories_data(list_models, variable = "BB0")
#' df
#' }
#' 
#' @export
#' @importFrom dplyr %>% rename mutate summarise ungroup
#' @importFrom stats median quantile
trajectories_data <- function(list_models, variable) {
  columns <- list(
    BB0 = "BB0",
    BBmsy = "stock",
    FFmsy = "harvest"
  )

  variable <- columns[[variable]]

  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(.data[[variable]]),
      lcl = quantile(.data[[variable]], probs = 0.025),
      ucl = quantile(.data[[variable]], probs = 0.975),
      lcl2 = quantile(.data[[variable]], probs = 0.1),
      ucl2 = quantile(.data[[variable]], probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}

#' Plot model trajectories
#'
#' Creates a ggplot2-based visualization of model trajectories over time,
#' including median trends and uncertainty intervals across scenarios.
#'
#' @param df A data frame as returned by \code{trajectories_data()}.
#' @param variable A character string indicating the variable to plot.
#' @param palette A character vector of colors used for plotting.
#'   Options are \code{"BB0"}, \code{"BBmsy"}, or \code{"FFmsy"}.
#' @param title_y A character string or expression for the y-axis label.
#'   Defaults to a mathematical expression depending on the selected variable.
#'
#' @return A ggplot object displaying trajectory summaries with
#'   confidence intervals, faceted by scenario.
#'
#' @details
#' The plot includes ribbons representing 80\% and 95\% confidence
#' intervals, a median trajectory line, and facets by scenario.
#' The y-axis label is automatically defined based on the selected variable
#' unless provided by the user.
#'
#' @examples
#' \dontrun{
#' df <- trajectories_data(out, variable = "BB0")
#' trajectories_ggplot(df, variable = "BB0", palette = c("blue"))
#' }
#'
#' @examples
#' \dontrun{
#' df <- trajectories_data(list_models, "BB0")
#' trajectories_ggplot(df, variable = "BB0", palette = c("#4285f4" "#34a853" "#ea4335"))
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line facet_wrap 
#' scale_y_continuous labs theme
trajectories_ggplot <- function(
  df, variable, palette = c("#4285f4", "#34a853", "#ea4335"), title_y = NULL
) {
  labels_y <- list(
    BB0 = expression(B/B[0]),
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY])
  )

  if (is.null(title_y)) {
    title_y <- labels_y[[variable]]
  }

  ggplot() +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2)) +
    geom_line(data = df, aes(x = year, y = mu),
              size = 1) +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Year", y = title_y, fill = "",
         colour = "") +
    .my_theme() +
    theme(legend.position = "none")
}