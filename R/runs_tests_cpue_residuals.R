#' Prepare runs test diagnostics data
#'
#' Processes model outputs to compute runs test diagnostics and residual
#' structures for CPUE indices, including confidence limits and LOESS
#' smoothing.
#'
#' @param list_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{cpue_residuals}{A data frame containing residuals, fitted
#'   LOESS values, and confidence bands.}
#'   \item{SE3}{A data frame containing runs test results, including
#'   lower and upper confidence limits and p-values.}
#'   \item{RMSE_data}{A data frame with RMSE-related diagnostics.}
#' }
#'
#' @details
#' The function computes runs tests using \code{JABBA::jbruns_sig3}
#' and classifies results based on statistical significance. Residuals
#' are smoothed using LOESS, and confidence intervals are derived
#' from the fitted model.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_models <- list(fit.S01, fit.S02)
#' df <- runs_tests_data(list_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% mutate filter left_join select
#' @importFrom JABBA jbruns_sig3
#' @importFrom stats loess predict complete.cases
#' @importFrom forcats fct_relevel
runs_tests_data <- function(list_models) {
  ###@> Filtering the expected data...
  .validate_fits_input_data(list_models)

  tmp05 <- .process_runs(list_models)

  #####@> Runstest...
  out.test <- data.frame(
    expand.grid(
      Index = names(tmp05)[4:ncol(tmp05)], 
      Scenario = unique(tmp05$Scenario),
      ymin = as.integer(1950), 
      ymax = as.integer(2023), 
      lcl = NA, 
      ucl = NA, 
      pvalue = NA
    )
  )
  
  for(i in 4:ncol(tmp05)) {
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
    tmp05, names_to = "Index", values_to = "Res", 4:ncol(tmp05)
  ) %>%
    filter(complete.cases(.)) %>%
    left_join(out.test, by = c("Scenario", "Index")) %>%
    select(Year:Res, lcl, ucl) %>%
    mutate(
      class = ifelse(Res < lcl | Res > ucl, "red", "white"),
      Index = fct_relevel(Index) # after add the option for the user to choose the order
    ) %>%
    droplevels()

  ###@> Including p-value in plot...
  out.test <- out.test %>%
      mutate(x = mean(1950:2023), y = 0.65) # adapt to be generic
  
  loess_fit <- loess(Res ~ Year, data = tmp05)
  tmp05$fit <- predict(loess_fit)
  pred <- predict(loess_fit, se = TRUE)

  tmp05$fit   <- pred$fit
  tmp05$upper <- pred$fit + 1.96 * pred$se.fit
  tmp05$lower <- pred$fit - 1.96 * pred$se.fit

  RMSE_data <- .process_stats(list_models) %>%
    filter(Stastistic == "RMSE")

  list(
    cpue_residuals = tmp05,
    SE3 = out.test,
    RMSE_data = RMSE_data
  )
}

#' Plot runs test diagnostics
#'
#' Creates a ggplot2-based visualization of runs test diagnostics,
#' including residuals, confidence limits, and p-values across
#' scenarios and indices.
#'
#' @param df_lists A named list as returned by \code{runs_tests_data()}.
#' @param title_y A character string for the y-axis label.
#'   Defaults to "Residuals".
#'
#' @return A ggplot object showing residuals, confidence regions,
#'   and runs test results.
#'
#' @details
#' The plot includes shaded regions representing confidence limits,
#' residual segments, highlighted points based on threshold exceedance,
#' and p-value annotations. Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' df <- runs_tests_data(list_models)
#' runs_tests_ggplot(df)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_rect aes geom_text geom_hline geom_segment 
#' geom_point facet_grid scale_fill_manual scale_y_continuous labs theme
runs_tests_ggplot <- function(df_lists, title_y = "Residuals") {
  ggplot() +
    geom_rect(data = df_lists$SE3,
              aes(xmin = ymin, xmax = ymax, ymin = lcl, ymax = ucl,
                  fill = class),
              alpha = 0.2) +
    geom_text(data = df_lists$SE3,
        aes(x = x, y = y, label = paste0("p-value = ", round(pvalue, 3)))) +
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
    scale_y_continuous(expand = c(0, 0), limits = c(-0.8, 0.8)) + # adapt to be generic
    labs(x = "Year", y = title_y) +
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
#' @param palette A character vector of colors used for plotting. 
#'   Defaults to the vector \code{c("#4285f4", "#34a853", "#ea4335")}
#' @param title_y A character string for the y-axis label. 
#'   Defaults to "Residuals"
#' 
#' @return A ggplot object displaying CPUE residual diagnostics.
#' 
#' @details 
#' The plot includes residual segments, observed values, a smoothed trend,
#' and RMSE annotations for each scenario.
#' 
#' @importFrom ggplot2 ggplot geom_hline geom_segment aes geom_point geom_smooth
#' geom_text facet_wrap scale_y_continuous scale_fill_manual scale_colour_manual 
#' labs theme
cpue_conflicts_ggplot <- function(
  df_lists, palette = c("#4285f4", "#34a853", "#ea4335"), title_y = "Residuals"
) {
  max_val <- .round_to_nearest(max(df_lists$cpue_residuals$Res, na.rm = TRUE), TRUE)
  min_val <- .round_to_nearest(min(df_lists$cpue_residuals$Res, na.rm = TRUE), FALSE)

  max_year <- max(df_lists$cpue_residuals$Year)
  min_year <- min(df_lists$cpue_residuals$Year)
  x_diff <- max_year - min_year
  x_year <- max_year - round(x_diff/4, 0)
  
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
              aes(x = x_year, 
                  y = max_val / 1.1, 
                  label = paste0("RMSE = ", Value, " %"))) +
    facet_wrap(~ Scenario, scales = "fixed", ncol = 3) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_val, max_val)) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    labs(x = "Year", y = title_y, fill = "", colour = "") +
    .my_theme() +
    theme(legend.position = "top")
}

#' Extract and combine residuals data
#' 
#' Internal helper that extracts residuals from each model, converts
#' them into a data frame format, and combines them across scenarios.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing residuals by year and scenario.
#' 
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_runs <- function(fit_list) {#, vars = "runs") {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Year = fit$yr,
        Scenario = fit$scenario,
        Ref = 0,
        t(fit$residuals)
        # if(vars == "runs") {
        #   t(fit$residuals)
        # } else {
        #   t(fit$residuals)
        # }
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

# #' Prepare CPUE conflict diagnostics data
# #'
# #' Internal helper that filters model statistics to extract RMSE values
# #' and prepares them for annotation in CPUE residual diagnostics plots.
# #'
# #' @param list_models A list of model outputs.
# #'
# #' @return A data frame containing RMSE values with plotting coordinates.
# #' 
# #' @keywords internal
# #' @importFrom dplyr mutate
# #' @importFrom dplyr %>% filter
# .cpue_conflicts_data <- function(list_models) {
#   .process_stats(list_models) %>%
#     filter(Stastistic == "RMSE")
# }

#' Extract and combine model statistics
#'
#' Internal helper that extracts summary statistics from each model
#' and combines them into a single data frame across scenarios.
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