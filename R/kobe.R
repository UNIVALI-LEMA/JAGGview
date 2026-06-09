#' Prepare data for Kobe plot visualization
#'
#' Computes biomass and fishing mortality ratios and prepares all required 
#' components to build a Kobe plot, including credibility contours and 
#' reference quadrants.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#' \code{harvest} (F/Fmsy), and \code{stock} (B/Bmsy), returned by the JABBA 
#' function \code{JABBA::jbplot_ensemble()}.
#' @param ci_levels A numeric vector containing the CI values between 0 and 1.
#' Deafults to 0.5, 0.8, 0.95
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
#' The function computes median biomass (B/Bmsy) and fishing mortality (F/Fmsy) 
#' ratios by year and scenario, and estimates kernel density contours for the 
#' terminal year using \code{gplots::ci2d}. These contours represent 
#' uncertainty regions commonly displayed in Kobe plots.
#'
#' The output is designed to be used directly with \code{kobe_ggplot()}.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- kobe_data(list_fit_models)
#' str(df)
#' }
#'
#' @export
#' @importFrom dplyr %>% summarise arrange filter
#' @importFrom stats median
#' @importFrom gplots ci2d
kobe_data <- function(list_fit_models, ci_levels = c(0.5, 0.8, 0.95)) {
  # ###@> Filtering the expected data...
  # .validate_jbplot_ensemble(model_results)
  model_results <- .jbplot_ensemble2(
    kb = list_fit_models,
    kbout = TRUE,
    plot = FALSE
  )

  if (!inherits(ci_levels, "numeric")) {
    stop("Parameter 'ci_levels' was expecting a numeric vector")
  }

  if(any(is.na(ci_levels))) {
    stop("Parameter 'ci_levels' cannot contain NA.")
  }

  if(any(ci_levels <= 0 | ci_levels >= 1)) {
    stop("Parameter 'ci_levels' was expecting numbers between 0 and 1.")
  }

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

  #####@> Extracting data...
  tmp11 <- model_results %>%
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

  max_year <- max(model_results$year)
  min_year <- min(model_results$year)
  mid_year <- round(min_year + (max_year - min_year)/2)

  tmp11b <- filter(tmp11, year %in% c(min_year, mid_year, max_year))
  tmp11c <- filter(model_results, year == max_year)

  k.out <- data.frame(x = NULL, y = NULL, Scenario = NULL, q = NULL)
  for(i in unique(model_results$Scenario)) {
    x <- filter(model_results, Scenario == i)
    x <- filter(x, year == max_year)
    kernelF <- ci2d(
      x$stock, 
      x$harvest, 
      nbins = 151,  # See if can be generic (seems more of a parameter)
      factor = 1.5, # See if can be generic (seems more of a parameter)
      ci.levels = ci_levels,
      show = "none",
      col = 1       # See if can be generic 
    )

    tmp00 <- lapply(
      ci_levels, function(ci) {
        q <- kernelF$contours[[as.character(ci)]]
        q$Scenario <- i
        q$q <- paste0(ci*100, "%")
        q
      })
    
    tmp <- do.call(rbind, tmp00)

    k.out <- rbind(
      k.out, 
      data.frame(
        x = tmp$x,
        y = tmp$y,
        Scenario = tmp$Scenario,
        q = fct_relevel(tmp$q, sort(unique(tmp$q), decreasing = TRUE))
      )
    )
  }
  
  results <- list(
    col01 = col01[1, , drop = FALSE],
    col02 = col02[1, , drop = FALSE],
    col03 = col03[1, , drop = FALSE],
    col04 = col04[1, , drop = FALSE],
    k.out = k.out,
    tmp11 = tmp11,
    tmp11b = tmp11b
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(is.na(results))) {
    stop("Data frame only have NA data.")
  }

  return(results)
}

#' Plot Kobe diagram
#'
#' Creates a Kobe plot showing the status of fish stocks in terms of biomass 
#' (B/Bmsy) and fishing mortality (F/Fmsy), including uncertainty contours and 
#' temporal trajectories.
#'
#' @param df_lists A list as returned by \code{kobe_data()}.
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
#' @export
#' @importFrom ggplot2 ggplot geom_rect aes geom_hline geom_vline geom_polygon 
#' geom_path geom_point facet_wrap scale_y_continuous scale_x_continuous 
#' scale_shape_manual scale_fill_manual labs coord_cartesian theme
#' @importFrom grDevices colorRampPalette
kobe_ggplot <- function(df_lists) {
  if(!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  max_x <- .round_to_nearest(max(df_lists$tmp11$Bratio, na.rm = TRUE), TRUE, 1)
  max_y <- .round_to_nearest(max(df_lists$tmp11$Bratio, na.rm = TRUE), TRUE, 1)
  if(max_x > 6) max_x <- 6
  if(max_y > 6) max_y <- 6

  n_levels <- length(unique(df_lists$k.out$q))
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
    geom_polygon(data = df_lists$k.out,
                 aes(x = x, y = y, fill = q),
                 colour = "gray30") +
    geom_path(data = df_lists$tmp11, aes(x = Bratio, y = Fratio)) +
    geom_point(data = df_lists$tmp11b,
               aes(x = Bratio, y = Fratio, shape = factor(year)),
               size = 4, fill = "white") +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(
      values = colorRampPalette(c("cornsilk4", "grey", "cornsilk2"))(n_levels)
    ) +
    labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]), fill = "",
         shape = "") +
    coord_cartesian(xlim = c(0, max_x), ylim = c(0, max_y)) +
    .my_theme() +
    theme(legend.position = "top")
}