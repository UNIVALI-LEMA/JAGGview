#' Prepare fitted index data and credibility intervals
#'
#' Processes model outputs to generate formatted data for fitted indices_factor,
#' including mean values and credibility intervals (80% and 95%).
#'
#' Extracts index and uncertainty information from a list of model results, 
#' computes upper and lower credibility bounds, and organizes the data for 
#' downstream visualization.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#'   JABBA function \code{JABBA::fit_jabba()}.
#' @param indices_factor Optional. A vector of indices_factor to include. Must 
#'   exist in the \code{Index} column.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{Li_Ui}{A data frame containing mean values and lower (Li) and
#'   upper (Ui) credibility bounds.}
#'   \item{CI_80}{A data frame containing fitted values and 80% credibility
#'   intervals.}
#'   \item{CI_95}{A data frame containing fitted values and 95% credibility
#'   intervals.}
#' }
#'
#' @details
#' The function processes scenario-based outputs for indices_factor (\code{I}),
#' standard errors (\code{SE2}), and fitted values (\code{ppd} and \code{hat}).
#' Missing values are handled using reference index data when necessary.
#'
#' Credibility intervals for \code{Li_Ui} are computed assuming normality,
#' using a multiplier of 1.96 applied to the standard error.
#'
#' If \code{indices_factor} is provided, the results are filtered and reordered
#' accordingly. The function also ensures consistency across different
#' model outputs and removes incomplete cases before returning results.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' result <- fits_data(list_fit_models)
#' result
#' }
#'
#' @export
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr %>% select filter full_join 
#' @importFrom stats complete.cases
#' @importFrom forcats fct_relevel
fits_data <- function(list_fit_models, indices_factor = NULL) {
  # ###@> Filtering the expected data...
  # .validate_fits_input_data(list_fit_models)
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  ###@> Index...
  tmp01 <- .process_scenarios(list_fit_models, vars = "I")
  
  ###@> SE...
  tmp02 <- .process_scenarios(list_fit_models, vars = "SE2")

  ##@> Replacing NA...
  tmp02 <- .replace_na_with_na(tmp02, tmp01)

  ####@> Merging tmp01 and tmp02...
  tmp01 <- pivot_longer(
    data = tmp01, names_to = "Index", values_to = "Mean", 3:ncol(tmp01)
  )
  tmp02 <- pivot_longer(
    data = tmp02, names_to = "Index", values_to = "SE", 3:ncol(tmp02)
  )
  tmp00 <- full_join(tmp01, tmp02)

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp00$Index), indices_factor)
  }

  ####@> Estimating Upper and Lower errors...
  tmp00$error <- with(tmp00, (1.96 * sqrt(SE)))
  tmp00$Ui <- with(tmp00, Mean + error)
  tmp00$Li <- with(tmp00, Mean - error)

  index_inputseries <- .process_index(list_fit_models)

  if (any(is.na(tmp00$Index))) .fill_na_indices(tmp00, index_inputseries)

  tmp00 <- tmp00[complete.cases(tmp00),] %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>%
    select(-SE)

  ####@> Fit (CI 80%)...
  tmp03 <- .process_cpues(list_fit_models, vars = "ppd")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp03$Index), indices_factor)
  }

  if (any(is.na(tmp03$Index))) .fill_na_indices(tmp03, index_inputseries)

  tmp03 <- tmp03 %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>% 
    select(-c(se, obserror))

  ####@> Fit (CI 95%)...
  tmp04 <- .process_cpues(list_fit_models, vars = "hat")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp04$Index), indices_factor)
  }

  if (any(is.na(tmp04$Index))) .fill_na_indices(tmp04, index_inputseries)

  tmp04 <- tmp04 %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>%
    select(-c(se, obserror, mu))

  results <- list(
    Li_Ui = tmp00,
    CI_80 = tmp03,
    CI_95 = tmp04
  )
  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }
  
  return(results)
}

#' Plot fitted indices with credibility intervals
#'
#' Creates a ggplot2-based visualization of fitted abundance indices,
#' including mean values and credibility intervals (80% and 95%).
#'
#' @param df_lists A named list of data frames as returned by
#'   \code{fits_data()}. It must contain the elements \code{Li_Ui},
#'   \code{CI_80}, and \code{CI_95}.
#' @param title_y A character string for the y-axis label.
#'   Defaults to "Abundance index".
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
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_errorbar facet_grid 
#' scale_y_continuous labs geom_point aes
fits_ggplot <- function(
  df_lists, title_y = "Abundance index", palette = NULL, x_lim = NULL, 
  y_lim = NULL
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
    geom_line(data = df_lists$CI_80,
        aes(x = Year, y = mu)) +
    geom_errorbar(data = df_lists$Li_Ui, 
                  aes(x = Year, ymin = Li, ymax = Ui), width = 1.5) +
    geom_point(data = df_lists$Li_Ui,
        aes(x = Year, y = Mean),
        pch = 21, fill = "white", size = 1.5) + 
    facet_grid(Scenario ~ Index, scales = "free") +
    coord_cartesian(xlim = x_lim, ylim = y_lim) + 
    labs(x = "Year", y = title_y) +
    .my_theme()
}

#' Extract and combine scenario-level data
#'
#' Internal helper that extracts scenario-specific variables from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#' @param vars A character string indicating which variable to extract. 
#'   Supported values are "I" (index) and "SE2" (variance).
#'
#' @return A data frame containing year, scenario, and extracted variables.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_scenarios <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Year = fit$yr,
      Scenario = fit$scenario,
      if (vars == "I") {
        fit$settings$I
      } else {
        fit$settings$SE2
      }
    )
  })
  temp01 <- lapply(fit_list, function(fit) {
    c("Year", "Scenario", unique(fit$diags$name))
  })
  tmp01 <- mapply(.rename_columns, temp00, temp01, SIMPLIFY = FALSE)
  result <- bind_rows(tmp01)
  return(result)
}

#' Fill missing index values
#'
#' Internal helper that replaces missing values in the Index column using the 
#' set of expected indices from the input series.
#'
#' @param data A data frame containing an Index column.
#' @param index_inputseries A character vector with expected index names.
#'
#' @return The input data frame with missing Index values filled.
#'
#' @keywords internal
.fill_na_indices <- function(data, index_inputseries) {
  index_data <- unique(data$Index)
  index_inputseries <- index_inputseries[!index_inputseries == "year"]
  NA_index <- setdiff(index_inputseries, index_data)
  data$Index[is.na(data$Index)] <- NA_index
}

#' Extract index names from model inputs
#'
#' Internal helper that extracts CPUE index names from the input series of each 
#' model in the list and returns the unique set of indices.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A character vector containing unique index names across models.
#'
#' @keywords internal
.process_index <- function(fit_list) {
  temp00 <- lapply(fit_list, function(fit) {
    names(fit$inputseries$cpue)
  })
  return(unique(unlist(temp00)))
}

#' Extract and combine CPUE data
#'
#' Internal helper that extracts CPUE-related outputs from a list of model 
#' results, converts array-based data into data frames, and combines them into 
#' a single structure.
#'
#' @param fit_list A list of model outputs.
#' @param vars A character string indicating which CPUE output to extract.
#'   Supported values are "ppd" and "hat".
#'
#' @return A data frame containing combined CPUE data across scenarios.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_cpues <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Scenario = fit$scenario,
      if (vars == "ppd") {
          .array_to_dataframe(fit$cpue.ppd)
      } else {
          .array_to_dataframe(fit$cpue.hat)
      }
    )
  })
  result <- bind_rows(temp00)
  return(result)
}

#' Rename data frame columns
#'
#' Internal helper to assign new column names to a data frame.
#'
#' @param df A data frame.
#' @param col_names A character vector with new column names.
#'
#' @return The data frame with renamed columns.
#'
#' @keywords internal
#' @importFrom stats setNames 
.rename_columns <- function(df, col_names) {
  setNames(df, col_names)
}