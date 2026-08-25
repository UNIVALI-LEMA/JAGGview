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
      df$indicator <- rownames(df)
      df$scenario <- fit$scenario

      rownames(df) <- NULL
      df[, c("scenario", "indicator", setdiff(names(df), 
      c("scenario", "indicator")))]
    }
  )
  temp00 <- bind_rows(temp00) 

  return(temp00)
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
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
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
    rename(Scenario = level)

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
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
get_rho <- function(df_lists) {
  return(df_lists$rho_data)
}