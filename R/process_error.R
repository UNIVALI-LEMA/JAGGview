#' Prepare process error summaries
#'
#' Summarises process error (log-scale biomass deviations) across
#' scenarios and years, computing central tendency and uncertainty
#' intervals.
#'
#' @param list_models A data frame containing model outputs, including
#' process error values (e.g., \code{Bdev}), scenario identifiers, and 
#' year, returned by the JABBA function \code{JABBA::jbplot_ensemble()}.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{year}{Year of the observation.}
#'   \item{Scenario}{Scenario identifier.}
#'   \item{mu}{Median process error.}
#'   \item{lcl}{Lower 95% confidence limit (2.5% quantile).}
#'   \item{ucl}{Upper 95% confidence limit (97.5% quantile).}
#'   \item{lcl2}{Lower 80% confidence limit (10% quantile).}
#'   \item{ucl2}{Upper 80% confidence limit (90% quantile).}
#' }
#'
#' @details
#' The function aggregates process error values by year and scenario,
#' computing median and quantile-based confidence intervals to
#' characterise uncertainty.
#'
#' @examples
#' \dontrun{
#' list_models <- jbplot_ensemble()
#' df <- process_error_data(list_models)
#' head(df)
#' }
#'
#' @export
#' @importFrom dplyr %>% rename mutate summarise ungroup
#' @importFrom stats median quantile
process_error_data <- function(list_models) {
  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(Bdev),
      lcl = quantile(Bdev, probs = 0.025),
      ucl = quantile(Bdev, probs = 0.975),
      lcl2 = quantile(Bdev, probs = 0.1),
      ucl2 = quantile(Bdev, probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}

#' Plot process error trajectories
#'
#' Creates a ggplot2-based visualization of process error over time,
#' including median trends and uncertainty intervals across scenarios.
#'
#' @param df A data frame as returned by \code{process_error_data()}.
#' @param palette A character vector of colors used for plotting.
#' @param title_y A character string for the y-axis label. Defaults to
#'   "Process Error on log(Biomass)".
#'
#' @return A ggplot object displaying process error trajectories with
#'   confidence intervals, faceted by scenario.
#'
#' @details
#' The plot includes ribbons representing 80% and 95% confidence
#' intervals, a median trend line, and a horizontal reference line at zero.
#'
#' @examples
#' \dontrun{
#' df <- process_error_data(list_models)
#' process_error_ggplot(df, palette = c("blue"))
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_hline geom_line facet_wrap 
#' scale_y_continuous labs
process_error_ggplot <- function(df, palette, title_y = "Process Error on log(Biomass)") {
  ggplot() +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl, ymax = ucl)) +
    geom_ribbon(data = df, fill = palette[1], alpha = 0.3,
                aes(x = year, ymin = lcl2, ymax = ucl2)) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_line(data = df, aes(x = year, y = mu),
              size = 1) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = 3) +
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    labs(x = "Year", y = title_y) +
    .my_theme()
}