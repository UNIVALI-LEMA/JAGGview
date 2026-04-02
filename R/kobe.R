#' Prepare data for Kobe plot visualization
#'
#' Computes biomass and fishing mortality ratios and prepares all
#' required components to build a Kobe plot, including confidence
#' contours and reference quadrants.
#'
#' @param list_models A data frame containing model outputs with at
#' least the following columns: \code{year}, \code{Scenario},
#' \code{harvest} (F/Fmsy), and \code{stock} (B/Bmsy), returned by 
#' the JABBA function \code{JABBA::jbplot_ensemble()}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{col01}{Data frame defining the yellow Kobe quadrant (overfished, 
#'   not overfishing).}
#'   \item{col02}{Data frame defining the orange quadrant (overfished and 
#'   overfishing).}
#'   \item{col03}{Data frame defining the red quadrant (severely 
#'   overfished and overfishing).}
#'   \item{col04}{Data frame defining the green quadrant (healthy stock 
#'   conditions).}
#'   \item{k.out}{Data frame with kernel density contours (50%, 80%, 95%) 
#'   for the terminal year.}
#'   \item{tmp11}{Time series of median biomass and fishing mortality 
#'   ratios by year and scenario.}
#'   \item{tmp11b}{Subset of selected years (e.g., 1950, 1986, 2023) for 
#'   highlighting points.}
#' }
#'
#' @details
#' The function computes median biomass (B/Bmsy) and fishing mortality
#' (F/Fmsy) ratios by year and scenario, and estimates kernel density
#' contours for the terminal year using \code{gplots::ci2d}. These
#' contours represent uncertainty regions commonly displayed in Kobe plots.
#'
#' The output is designed to be used directly with
#' \code{kobe_ggplot()}.
#'
#' @examples
#' \dontrun{
#' list_models <- jbplot_ensemble()
#' df <- kobe_data(list_models)
#' str(df)
#' }
#'
#' @export
#' @importFrom dplyr %>% summarise arrange filter
#' @importFrom stats median
#' @importFrom gplots ci2d
kobe_data <- function(list_models) {
  #####@> Extracting data...
  tmp11 <- list_models %>%
    summarise(
      Fratio = median(harvest),
      Bratio = median(stock),
      .by = c(year, Scenario)
    ) %>%
    arrange(Scenario, year)
  col01 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(0, 0), ymax = c(1, 1), 
    col = "yellow"
  )
  col02 <- data.frame(
    xmin = c(1, 1), xmax = c(6, 6), ymin = c(1, 1), ymax = c(6, 6), 
    col = "orange"
  )
  col03 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(1, 1), ymax = c(6, 6), 
    col = "red"
  )
  col04 <- data.frame(
    xmin = c(1, 1), xmax = c(6, 6), ymin = c(0, 0), ymax = c(1, 1), 
    col = "#00FF00"
  )
  tmp11b <- filter(tmp11, year %in% c(1950, 1986, 2023))
  tmp11c <- filter(list_models, year == 2023)

  k.out <- data.frame(x = NULL, y = NULL, Scenario = NULL, q = NULL)
  for(i in unique(list_models$Scenario)) {
    x <- filter(list_models, Scenario == i)
    x <- filter(x, year == 2023)
    kernelF <- ci2d(
      x$stock, 
      x$harvest, 
      nbins = 151, 
      factor = 1.5, 
      ci.levels = c(0.5, 0.8, 0.95),
      show = "none",
      col = 1
    )
    q50 <- kernelF$contours$"0.5"
    q50$Scenario <- i; q50$q <- "50%"
    q80 <- kernelF$contours$"0.8"
    q80$Scenario <- i; q80$q <- "80%"
    q95 <- kernelF$contours$"0.95"
    q95$Scenario <- i; q95$q <- "95%"
    tmp <- rbind(q50, q80, q95)
    k.out <- rbind(
      k.out, 
      data.frame(
        x = tmp$x,
        y = tmp$y,
        Scenario = tmp$Scenario,
        q = tmp$q
      )
    )
  }
  list(
    col01 = col01[1, , drop = FALSE],
    col02 = col02[1, , drop = FALSE],
    col03 = col03[1, , drop = FALSE],
    col04 = col04[1, , drop = FALSE],
    k.out = k.out,
    tmp11 = tmp11,
    tmp11b = tmp11b
  )
}

#' Plot Kobe diagram
#'
#' Creates a Kobe plot showing the status of fish stocks in terms of
#' biomass (B/Bmsy) and fishing mortality (F/Fmsy), including
#' uncertainty contours and temporal trajectories.
#'
#' @param df A list as returned by \code{kobe_data()}.
#'
#' @return A ggplot object representing the Kobe plot, faceted by scenario.
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Colored quadrants representing stock status regions.
#'   \item Kernel density contours (50\%, 80\%, 95\%) for uncertainty.
#'   \item Time series trajectory of stock status.
#'   \item Highlighted reference years.
#'   \item Reference lines at B/Bmsy = 1 and F/Fmsy = 1.
#' }
#'
#' Faceting is applied by scenario, allowing comparison across model runs.
#'
#' @examples
#' \dontrun{
#' df <- kobe_data(list_models)
#' kobe_ggplot(df)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_rect aes geom_hline geom_vline 
#' geom_polygon geom_path geom_point facet_wrap scale_y_continuous
#' scale_x_continuous scale_shape_manual scale_fill_manual labs
#' coord_cartesian theme
kobe_ggplot <- function(df) {
  ggplot() +
    geom_rect(data = df$col01, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "yellow") +
    geom_rect(data = df$col02, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "orange") +
    geom_rect(data = df$col03, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "red") +
    geom_rect(data = df$col04, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "green") +
    geom_hline(yintercept = 1, linetype = "longdash") +
    geom_vline(xintercept = 1, linetype = "longdash") +
    geom_polygon(
      data = df$k.out,
      aes(x = x, y = y, fill = factor(q, levels = c("95%", "80%", "50%"))),
      colour = "gray30") +
    geom_path(data = df$tmp11, aes(x = Bratio, y = Fratio)) +
    geom_point(data = df$tmp11b,
               aes(x = Bratio, y = Fratio, shape = factor(year)),
               size = 4, fill = "white") +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(values = c("cornsilk4", "grey", "cornsilk2")) +
    labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]), fill = "",
         colour = "", shape = "") +
    coord_cartesian(xlim = c(0, 4), ylim = c(0, 3)) +
    .my_theme() +
    theme(legend.position = "top")
}