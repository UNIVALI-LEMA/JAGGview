#' Extract MASE data from hindcast results
#' 
#' Retrieves the data frame containing MASE (Mean Absolute Scaled Errors) 
#' values for all indices and scenarios, as returned by \code{hindcast_data()}.
#' 
#' @param df_lists A named list object returned by \code{hindcast_data()}, 
#'   which must contain a component named \code{"mase_data"}.
#' 
#' @return A data frame containing Mean Absolute Scaled Error (MASE) metrics 
#' for each index and scenario.
#' 
#' @details
#' The returned data frame is in wide format, with one row per combination of 
#' Index and Scenario. This function is a convenience acessor for extracting
#' MASE results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' df <- hindcast_data(list_hc_models)
#' get_mase(df)
#' }
#' 
#' @family extraction functions
#' @family hindcasts functions
#' 
#' @export
get_mase <- function(df_lists) {
  return(df_lists$mase_data)
}

#' Extract parameters data from fitted models
#' 
#' Retrieves a combined data frame containing model parameters extracted from
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A data frame containing parameter estimates for each model, 
#'   including the parameter name and associated scenario.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a 
#' list to ensure consistent processing. For each model, the \code{pars} 
#' component is converted to a data frame, with row names extracted as an 
#' \code{indicator} column and the model \code{scenario} appended as an 
#' additional column.
#' 
#' The resulting data frames are combined by rows into a single data frame,
#' facilitating comparison of parameter estimates across scenarios or models.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_pars(list_fit_models)
#' }
#' 
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
get_pars <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(
    list_fit_models,
    function(fit) {
      df <- as.data.frame(fit$pars)
      df$Indicator <- rownames(df)
      df$Scenario <- fit$scenario

      rownames(df) <- NULL
      df[, c("Scenario", "Indicator", setdiff(names(df), 
      c("Scenario", "Indicator")))]
    }
  )
  temp02 <- bind_rows(temp00) 

  return(temp02)
}

#' Extract PPMR data by scenario
#'
#' Retrieves the data frame containing PPMR (Posterior Probability of Metric 
#' exceeding a reference) values for all indicators and scenarios, as returned 
#' by \code{priors_posteriors_data()}.
#'
#' @param df_lists A named list object returned by 
#'   \code{priors_posteriors_data()}, which must contain a component named 
#'   \code{"PPMR"}.
#'
#' @return A data frame with the following structure:
#' \describe{
#'   \item{Scenario}{Scenario identifier}
#'   \item{K, r, psi, ...}{Numeric PPMR values for each indicator}
#'  }
#'
#' @details
#' The returned data frame is in wide format, with one row per scenario and one 
#' column per indicator. This function is a convenience accessor for extracting 
#' PPMR results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_pr(list_fit_models)
#' }
#' 
#' @family extraction functions
#' @family priors vs posteriors functions
#'
#' @export
get_ppmr <- function(df_lists) {
  return(df_lists$PPMR)
}

#' Extract PPVR data from priors/posteriors results
#'
#' Retrieves the data frame containing PPVR (Posterior Probability of
#' Variable in a reference region) values for all indicators and scenarios, as 
#' returned by \code{priors_posteriors_data()}.
#'
#' @param df_lists A named list object returned by
#'   \code{priors_posteriors_data()}, which must contain a component named 
#'   \code{"PPVR"}.
#'
#' @return A data frame with the following structure:
#' \describe{
#'   \item{Scenario}{Scenario identifier}
#'   \item{K, r, psi, ...}{Numeric PPVR values for each indicator}
#'  }
#'
#' @details
#' The returned data frame is in wide format, with one row per scenario and one 
#' column per indicator. This function is a convenience accessor for extracting 
#' PPVR results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_ppvr(list_fit_models)
#' }
#' 
#' @family extraction functions
#' @family priors vs posteriors functions
#'
#' @export
get_ppvr <- function(df_lists) {
  return(df_lists$PPVR)
}

#' Extract reference points data from fitted models
#' 
#' Retrieves the data frame containing reference points (refpts) extracted from
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A combined data frame containing reference points (refpts) for all
#'   fitted models provided in \code{list_fit_models}.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a list
#' to ensure consistent processing. The function extracts the \code{refpts}
#' component from each model and binds them by rows into a single data frame.
#' 
#' This function is a convenience accessor to facilitate comparison and further
#' analysis of reference points across multiple fitted models.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_refpts(lits_fit_models)
#' }
#' 
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr across bind_rows mutate rename
get_refpts <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(list_fit_models, function(fit) fit$refpts)

  temp00 <- bind_rows(temp00) %>%
    mutate(
      across(
        c(k, bmsy, fmsy, msy),
        ~ifelse(quant == "logse", exp(.x), .x)
      )
    ) %>%
    rename(Scenario = level, Factor = factor, Quant = quant)

  return(temp00)
}

#' Extract rho data from retrospective analysis results
#' 
#' Retrieves the data frame containing rho (retrospective bias metrics) values
#' for all indices and scenarios, as returned by 
#' \code{retrospective_analysis_data()}.
#' 
#' @param df_lists A named list object returned by 
#'   \code{retrospective_analysis_data()}, which must contain a component named 
#'   \code{"rho_data"}.
#' 
#' @return A data frame where each row represents a combination of scenario and
#'   index, typically including the following columns:
#' \describe{
#'   \item{Scenario}{Scenario identifier}
#'   \item{Index}{Short name of the indicator (e.g., \code{B}, \code{F}, 
#'   \code{BBmsy})}
#'   \item{Index2}{Descriptive name of the indicator}
#'   \item{rho}{Numeric value representing retrospective bias for the given 
#'   index}
#' }
#' 
#' @details
#' The returned data frame is in long format, with one row per combination of
#' scenario and index. The \code{rho} metric represents retrospective bias,
#' where values close to zero indicate low bias, positive values indicate
#' overestimation, and negative values indicate underestimation.
#' 
#' This function is a convenience acessor for extracting retrospective analysis
#' results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' df <- retrospective_analysis_data(list_hc_models)
#' get_rho(df)
#' }
#' 
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
get_rho <- function(df_lists) {
  temp00 <- df_lists$rho_data %>% select(Scenario, Index, rho)

  temp01 <- pivot_wider(
    temp00, names_from = "Index", values_from = "rho"
  )
  
  return(temp01)
}

#' Extract fitted estimates from fitted models
#' 
#' Retrieves a combined data frame containing fitted estimates extracted from
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A data frame containing fitted estimates for each model, including
#'   the associated scenario and indicator name.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a 
#' list to ensure consistent processing. For each model, the \code{estimates}
#' component is converted to a data frame, with row names extracted as an
#' \code{Indicator} column and the model \code{Scenario} appended as an
#' additional column.
#' 
#' The resulting data frames are combined by rows into a single data frame,
#' facilitating comparison of fitted estimates across scenarios or models.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_estimates(list_fit_models)
#' }
#' 
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows rename
get_estimates <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(
    list_fit_models,
    function(fit) {
      df <- as.data.frame(fit$estimates)
      df$Indicator <- rownames(df)
      df$Scenario <- fit$scenario

      rownames(df) <- NULL
      df[, c("Scenario", "Indicator", setdiff(names(df), 
      c("Scenario", "Indicator")))]
    }
  )
  temp02 <- bind_rows(temp00) %>%
    rename(LCI = lci, UCI = uci)

  return(temp02)
}

#' Extract summary from fitted models
#' 
#' Retrieves and reshapes the summary statistics (\code{stats}) extracted from 
#' one or more fitted JABBA models returned by \code{fit_jabba()}.
#' 
#' @param list_fit_models A list of fitted model objects returned by 
#'   \code{fit_jabba()}, or a single fitted model object.
#' 
#' @return A data frame in wide format, with one row per scenario and one 
#'   column for each statistic.
#' 
#' @details
#' If a single fitted model is provided, it is automatically wrapped into a 
#' list to ensure consistent processing. The function extracts the \code{stats}
#' component from each model, adds a \code{scenario} column identifying the
#' model of origin, and binds them by rows into a single long-format data
#' frame. This combined data frame is then pivoted to wide format, with
#' statistic names taken from the \code{Stastistic} column becoming individual
#' columns and their corresponding \code{Value} entries populating the cells.
#' 
#' This function is a convenience accessor to facilitate comparison of summary
#' statistics across multiple fitted models.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' get_stats(list_fit_models)
#' }
#' 
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
get_stats <- function(list_fit_models) {
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  temp00 <- lapply(
    list_fit_models,
    function(fit) {
      df <- as.data.frame(fit$stats)
      df$Scenario <- fit$scenario

      rownames(df) <- NULL
      df[, c("Scenario", setdiff(names(df), "Scenario"))]
    }
  )
  temp02 <- bind_rows(temp00)

  temp03 <- pivot_wider(
    temp02, names_from = "Stastistic", values_from = "Value"
  )

  return(as.data.frame(temp03))
}

#' Extract parameters data from hindcast retrospective models
#' 
#' Retrieves a combined data frame containing model parameters extracted from
#' one or more sets of hindcast retrospective models returned by 
#' \code{hindcast_jabba()}.
#' 
#' @param list_hc_models A list of hindcast model objects returned by 
#'   \code{hindcast_jabba()}, or a single hindcast model object.
#' 
#' @return A data frame containing parameter estimates for each peel and model,
#'  including the parameter name and associated scenario.
#' 
#' @details
#' If a single hindcast model object is provided, it is automatically wrapped
#' into a list to ensure consistent processing. For each hindcast model, the
#' function iterates over all retrospective peels, extracting the \code{pars}
#' component and appending the corresponding \code{scenario} and peel
#' identifiers. Row names from the \code{pars} component are extracted as an
#' \code{Indicator} column.
#' 
#' The resulting data frames are combined by rows into a single data frame,
#' facilitating comparison of parameter estimates across peels, scenarios, or
#' models.
#' 
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' get_hc_pars(list_hc_models)
#' }
#' 
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
#' @importFrom dplyr bind_rows relocate
get_hc_pars <- function(list_hc_models) {
  if (.is_hindcast_jabba(list_hc_models)) {
    list_hc_models <- list(list_hc_models)
  }

  temp00 <- lapply(
    list_hc_models,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          df <- data.frame(
            Scenario = hc[[nm]]$scenario,
            Peel = nm,
            hc[[nm]]$pars
          )
          df$Indicator <- rownames(df)
          rownames(df) <- NULL
          df %>%
            relocate(Scenario, Peel, Indicator)
        }
      )
      bind_rows(temp01)
    }
  )

  temp02 <- bind_rows(temp00)

  return(temp02)
}

#' Extract summary statistics from hindcast retrospective models
#' 
#' Retrieves and reshapes the summary statistics extracted from one or more
#' sets of hindcast retrospective models returned by \code{hindcast_jabba()}.
#' 
#' @param list_hc_models A list of hindcast model objects returned by 
#'   \code{hindcast_jabba()}, or a single hindcast model object.
#' 
#' @return A data frame in wide format, with one row per combination of  
#' scenario and peel, having one column for each statistic.
#' 
#' @details
#' If a single hindcast model object is provided, it is automatically wrapped
#' into a list to ensure consistent processing. For each hindcast model, the
#' function iterates over all retrospective peels, extracting the \code{stats}
#' component and appending the corresponding \code{scenario} and peel
#' identifiers. The combined long-format data frame is then pivoted to wide
#' format, with statistic names taken from the \code{Stastistic} column
#' becoming individual columns and their corresponding \code{Value} entries
#' populating the cells.
#' 
#' This function is a convenience accessor to facilitate comparison of summary
#' statistics across peels, scenarios, or models.
#' 
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' get_hc_stats(list_hc_models)
#' }
#' 
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
get_hc_stats <- function(list_hc_models) {
  if (.is_hindcast_jabba(list_hc_models)) {
    list_hc_models <- list(list_hc_models)
  }

  temp00 <- lapply(
    list_hc_models,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          data.frame(
            Scenario = hc[[nm]]$scenario,
            Peel = nm,
            hc[[nm]]$stats
          )
        }
      )
      bind_rows(temp01)
    }
  )
  temp02 <- bind_rows(temp00)

  temp03 <- pivot_wider(
    temp02, names_from = "Stastistic", values_from = "Value"
  )

  return(as.data.frame(temp03))
}


#' Extract fitted estimates from hindcast retrospective models
#' 
#' Retrieves a combined data frame containing fitted estimates extracted from
#' one or more sets of hindcast retrospective models returned by 
#' \code{hindcast_jabba()}.
#' 
#' @param list_hc_models A list of hindcast model objects returned by 
#'   \code{hindcast_jabba()}, or a single hindcast model object.
#' 
#' @return A data frame containing fitted estimates for each peel, including 
#'   the associated scenario and indicator name.
#' 
#' @details
#' If a single hindcast model object is provided, it is automatically wrapped
#' into a list to ensure consistent processing. For each hindcast model, the
#' function iterates over all retrospective peels, extracting the 
#' \code{estimates} component and appending the corresponding \code{scenario}
#' and peel identifiers. Row names from the \code{estimates} component are
#' extracted as an \code{Indicator} column.
#' 
#' The resulting data frames are combined by rows into a single data frame,
#' facilitating comparison of fitted estimates across peels, scenarios, or
#' models.
#' 
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' get_hc_estimates(list_hc_models)
#' }
#' 
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
#' @importFrom dplyr bind_rows relocate rename
get_hc_estimates <- function(list_hc_models) {
  if (.is_hindcast_jabba(list_hc_models)) {
    list_hc_models <- list(list_hc_models)
  }

  temp00 <- lapply(
    list_hc_models,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          df <- data.frame(
            Scenario = hc[[nm]]$scenario,
            Peel = nm,
            hc[[nm]]$estimates
          )
          df$Indicator <- rownames(df)
          rownames(df) <- NULL
          df %>%
            relocate(Scenario, Peel, Indicator)
        }
      )
      bind_rows(temp01)
    }
  )

  temp02 <- bind_rows(temp00) %>%
    rename(LCI = lci, UCI = uci)

  return(temp02)
}