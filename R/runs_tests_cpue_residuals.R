#' Prepare runs test diagnostics data
#'
#' Processes model outputs to compute runs test diagnostics and residual
#' structures for CPUE indices, including credibility limits and LOESS
#' smoothing.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#'   JABBA function \code{JABBA::fit_jabba()}.
#' @param indices_factor Optional. A vector of indices to include. Must exist
#'   in the \code{Index} column.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{cpue_residuals}{A data frame containing residuals, fitted
#'   LOESS values, and credibility bands.}
#'   \item{SE3}{A data frame containing runs test results, including
#'   lower and upper credibility limits and p-values.}
#'   \item{RMSE_data}{A data frame with RMSE-related diagnostics.}
#' }
#'
#' @details
#' The function computes runs tests using \code{JABBA::jbruns_sig3} and 
#' classifies results based on statistical significance. Residuals are smoothed 
#' using LOESS, and credibility intervals are derived from the fitted model.
#' 
#' If \code{indices_factor} is provided, the results are filtered and reordered
#' accordingly. The function also ensures consistency across different model 
#' outputs and removes incomplete cases before returning results.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- runs_tests_data(list_fit_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% mutate filter left_join select
#' @importFrom JABBA jbruns_sig3
#' @importFrom stats loess predict complete.cases
#' @importFrom forcats fct_relevel
runs_tests_data <- function(list_fit_models, indices_factor = NULL) {
  # ###@> Filtering the expected data...
  # .validate_fits_input_data(list_fit_models)
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  tmp05 <- .process_runs(list_fit_models)

  indices <- 4:ncol(tmp05)

  min_year <- min(tmp05$Year, na.rm = TRUE)

  max_year <- max(tmp05$Year, na.rm = TRUE)

  #####@> Runstest...
  out.test <- data.frame(
    expand.grid(
      Index = names(tmp05)[indices], 
      Scenario = unique(tmp05$Scenario),
      ymin = as.integer(min_year), 
      ymax = as.integer(max_year), 
      lcl = NA, 
      ucl = NA, 
      pvalue = NA
    )
  )
  
  for(i in indices) {
    for(j in unique(tmp05$Scenario)) {
      name <- names(tmp05)[i]
      index <- tmp05[tmp05$Scenario == j, i]
      index <- index[complete.cases(index)]
      test <- jbruns_sig3(index, type = "resid")
      out.test$lcl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[1]
      out.test$ucl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[2]
      out.test$pvalue[out.test$Index == name &
                      out.test$Scenario == j] <- test$p.runs
    }
  }
  out.test$class <- ifelse(out.test$pvalue < 0.05, "red", "green")
  out.test <- out.test[complete.cases(out.test),]

  ####@> Pivoting table...
  tmp05 <- pivot_longer(
    tmp05, names_to = "Index", values_to = "Res", indices
  ) 

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp05$Index), indices_factor)
  }
  
  tmp05 <- tmp05 %>%
    filter(complete.cases(.)) %>%
    left_join(out.test, by = c("Scenario", "Index")) %>%
    select(Year:Res, lcl, ucl) %>%
    mutate(
      class = ifelse(Res < lcl | Res > ucl, "red", "white"),
      Index = fct_relevel(Index, indices_factor)
    ) %>%
    droplevels()
  
  loess_fit <- loess(Res ~ Year, data = tmp05)
  tmp05$fit <- predict(loess_fit)
  pred <- predict(loess_fit, se = TRUE)

  tmp05$fit   <- pred$fit
  tmp05$upper <- pred$fit + 1.96 * pred$se.fit
  tmp05$lower <- pred$fit - 1.96 * pred$se.fit

  RMSE_data <- .process_stats(list_fit_models) %>%
    filter(Stastistic == "RMSE")

  results <- list(
    cpue_residuals = tmp05,
    SE3 = out.test,
    RMSE_data = RMSE_data
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Plot runs test diagnostics
#'
#' Creates a ggplot2-based visualization of runs test diagnostics, including 
#' residuals, credibility limits, and p-values across scenarios and indices.
#'
#' @param df_lists A named list as returned by \code{runs_tests_data()}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 4.
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
#' @export
#' @importFrom ggplot2 ggplot geom_rect aes geom_hline geom_segment geom_text
#' geom_point facet_grid scale_fill_manual scale_y_continuous labs theme
runs_tests_ggplot <- function(
  df_lists, text_size = 4, title_x = "Year", title_y = "Residuals", x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$SE3$ucl, na.rm = TRUE), TRUE, 2.5)
    min_y_val <- .round_to_nearest(min(df_lists$SE3$lcl, na.rm = TRUE), FALSE,2.5)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$SE3$ymax)
    min_x_val <- min(df_lists$SE3$ymin)
    x_lim <- c(min_x_val, max_x_val)
  }

  pos <- .auto_text_position(
    data_list = df_lists$cpue_residuals,
    col_x = "Year",
    col_y = "Res",
    margin = 0.4,
    xlim = x_lim,
    ylim = y_lim
  )

  ggplot() +
    geom_rect(data = df_lists$SE3,
              aes(xmin = ymin, xmax = ymax, ymin = lcl, ymax = ucl, 
                  fill = class),
              alpha = 0.2) +
    geom_text(data = df_lists$SE3,
        aes(x = pos$x, y = pos$y,
          label = paste0("p-value = ", round(pvalue, 3))), size = text_size) +
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

#' Plot CPUE residuals diagnostics
#' 
#' Creates a ggplot2- based visualization of CPUE residuals across
#' years and scenarios, including reference lines, residual segments,
#' smoothed trends, and RMSE annotations.
#' 
#' @param df_lists A named list of data frames as returned by
#'   \code{runs_tests_data()}.
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 4.
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
#' @export
#' @importFrom ggplot2 ggplot geom_hline geom_segment aes geom_point geom_smooth
#' facet_wrap scale_y_continuous scale_fill_manual scale_colour_manual labs 
#' theme geom_text
#' @importFrom grDevices colorRampPalette
cpue_residuals_ggplot <- function(
  df_lists, n_col = 3, text_size = 4, title_x = "Year", title_y = "Residuals", 
  palette = NULL, x_lim = NULL, y_lim = NULL
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
  
  pos <- .auto_text_position(
    data_list = df_lists$cpue_residuals,
    col_x = "Year",
    col_y = "Res",
    xlim = x_lim,
    ylim = y_lim,
    margin = 0.25
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
    geom_text(data = df_lists$RMSE_data,
              aes(x = pos$x, y = pos$y,
                  label = paste0("RMSE = ", Value, " %")), size = text_size) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = n_col) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    labs(x = title_x, y = title_y, fill = "", colour = "") +
    .my_theme() +
    theme(legend.position = "top")
}

#' Extract and combine residuals data
#' 
#' Internal helper that extracts residuals from each model, converts them into 
#' a data frame format, and combines them across scenarios.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing residuals by year and scenario.
#' 
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_runs <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Year = fit$yr,
        Scenario = fit$scenario,
        Ref = 0,
        t(fit$residuals)
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract and combine model statistics
#'
#' Internal helper that extracts summary statistics from each model and 
#' combines them into a single data frame across scenarios.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing model statistics by scenario.
#' 
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_stats <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        fit$stats
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}