#' Extract fitted estimates from fitted models
#' 
#' Extracts the parameter estimates of one or more JABBA fits and combines them 
#' into a single data frame, with one row per scenario and indicator. The 
#' result can be displayed with \code{\link{summary_table}()}. 
#'
#' @details
#' For each fit, the \code{estimates} component is converted to a data frame.
#' The row names become the \code{Indicator} column, and the \code{Scenario} of 
#' the fit is added as another column. The data frames of all fits are then 
#' combined by rows, which makes it easier to compare the estimates across 
#' scenarios. The columns \code{lci} and \code{uci} are renamed to \code{LCI} 
#' and \code{UCI}.
#' 
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario). A
#'   single fit is automatically wrapped into a list.
#' 
#' @return A data frame with one row per scenario and indicator. The first two 
#'   columns are \code{Scenario} and \code{Indicator}, followed by the columns 
#'   of the \code{estimates} component of the fits (including \code{LCI} and 
#'   \code{UCI}). Returns \code{NULL} if \code{list_fit_models} is \code{NULL} 
#'   or an empty list.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # A single fit
#' get_estimates(fit.S01)
#'
#' # A list of fits (one per scenario)
#' get_estimates(list(fit.S01, fit.S02))
#'
#' # Display as a table
#' summary_table(get_estimates(list(fit.S01, fit.S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr %>% bind_rows rename
get_estimates <- function(list_fit_models) {
  if (is.null(list_fit_models) || identical(list_fit_models, list())) {
    return(NULL)
  }
  
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

#' Extract fitted estimates from hindcast retrospective models
#' 
#' Extracts the parameter estimates of each retrospective run (peel) of one
#' or more JABBA hindcasts and combines them into a single data frame, with
#' one row per scenario, peel and indicator. The result can be displayed with
#' \code{\link{summary_table}()}.
#'
#' @details
#' For each hindcast, the function goes through all of its retrospective runs
#' (the elements of the object, named after the peels). The \code{estimates}
#' component of each run is converted to a data frame, the row names become
#' the \code{Indicator} column, and the \code{Scenario} and \code{Peel} are
#' added as other columns. The data frames of all runs and hindcasts are then
#' combined by rows, which makes it easier to compare the estimates across
#' peels and scenarios. The columns \code{lci} and \code{uci} are renamed to
#' \code{LCI} and \code{UCI}.
#' 
#' @param list_hc_models Either a single hindcast returned by
#'   \code{JABBA::hindcast_jabba()}, or a list of such hindcasts (one per
#'   scenario). A single hindcast is automatically wrapped into a list.
#' 
#' @return A data frame with one row per scenario, peel and indicator. The
#'   first three columns are \code{Scenario}, \code{Peel} and \code{Indicator}, 
#'   followed by the columns of the \code{estimates} component (including 
#'   \code{LCI} and \code{UCI}). Returns \code{NULL} if \code{list_hc_models} 
#'   is \code{NULL} or an empty list.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # A single hindcast
#' get_hc_estimates(hc_S01)
#'
#' # A list of hindcasts (one per scenario)
#' get_hc_estimates(list(hc_S01, hc_S02))
#'
#' # Display as a table
#' summary_table(get_hc_estimates(list(hc_S01, hc_S02)))
#' }
#'
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr %>% bind_rows relocate rename
get_hc_estimates <- function(list_hc_models) {
  if (is.null(list_hc_models) || identical(list_hc_models, list())) {
    return(NULL)
  }
  
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

#' Extract parameters data from hindcast retrospective models
#' 
#' Extracts the model parameters of each retrospective run (peel) of one or 
#' more JABBA hindcasts and combines them into a single data frame, with one
#' row per scenario, peel and parameter. The result can be displayed with
#' \code{\link{summary_table}()}.
#'
#' @details
#' For each hindcast, the function goes through all of its retrospective runs
#' (the elements of the object, named after the peels). The \code{pars}
#' component of each run is converted to a data frame, the row names become
#' the \code{Indicator} column, and the \code{Scenario} and \code{Peel} are
#' added as other columns. The data frames of all runs and hindcasts are then
#' combined by rows, which makes it easier to compare the parameters across
#' peels and scenarios.
#' 
#' @param list_hc_models Either a single hindcast returned by
#'   \code{JABBA::hindcast_jabba()}, or a list of such hindcasts (one per
#'   scenario). A single hindcast is automatically wrapped into a list.
#' 
#' @return A data frame with one row per scenario, peel and parameter. The
#'   first three columns are \code{Scenario}, \code{Peel} and \code{Indicator}, 
#'   followed by the columns of the \code{pars} component. Returns \code{NULL} 
#'   if \code{list_hc_models} is \code{NULL} or an empty list.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # A single hindcast
#' get_hc_pars(hc_S01)
#'
#' # A list of hindcasts (one per scenario)
#' get_hc_pars(list(hc_S01, hc_S02))
#'
#' # Display as a table
#' summary_table(get_hc_pars(list(hc_S01, hc_S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr %>% bind_rows relocate
get_hc_pars <- function(list_hc_models) {
  if (is.null(list_hc_models) || identical(list_hc_models, list())) {
    return(NULL)
  }
  
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
#' Extracts the summary statistics of each retrospective run (peel) of one or
#' more JABBA hindcasts and combines them into a single data frame in wide
#' format, with one row per scenario and peel and one column per statistic.
#' The result can be displayed with \code{\link{summary_table}()}.
#'
#' @details
#' For each hindcast, the function goes through all of its retrospective runs
#' (the elements of the object, named after the peels). The \code{stats}
#' component of each run is converted to a data frame, and the \code{Scenario} 
#' and \code{Peel} are added as other columns. The data frames of all runs and 
#' hindcasts are combined by rows and then pivoted to wide format: the names in 
#' the \code{Stastistic} column become the columns, and the \code{Value} 
#' entries fill the cells. This makes it easier to compare the statistics 
#' across peels and scenarios.
#'
#' @param list_hc_models Either a single hindcast returned by
#'   \code{JABBA::hindcast_jabba()}, or a list of such hindcasts (one per
#'   scenario). A single hindcast is automatically wrapped into a list.
#' 
#' @return A data frame in wide format, with one row per scenario and peel.
#'   The first two columns are \code{Scenario} and \code{Peel}, followed by
#'   one column for each statistic. Returns \code{NULL} if
#'   \code{list_hc_models} is \code{NULL} or an empty list.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # A single hindcast
#' get_hc_stats(hc_S01)
#'
#' # A list of hindcasts (one per scenario)
#' get_hc_stats(list(hc_S01, hc_S02))
#'
#' # Display as a table
#' summary_table(get_hc_stats(list(hc_S01, hc_S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
get_hc_stats <- function(list_hc_models) {
  if (is.null(list_hc_models) || identical(list_hc_models, list())) {
    return(NULL)
  }
  
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

  temp04 <- as.data.frame(temp03)

  return(temp04)
}

#' Extract MASE data from hindcast results
#' 
#' Extracts the Mean Absolute Scaled Error (MASE) of each index and scenario 
#' from the output of \code{\link{hindcast_data}()}. The result can be
#' displayed with \code{\link{summary_table}()}.
#'
#' @details
#' The MASE is a measure of the prediction skill of the hindcast. The data
#' frame has one row per combination of scenario and index. Indices whose
#' MASE is undefined are already removed by \code{hindcast_data()}.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{hindcast_data}()}, with the element \code{mase_data}.
#'
#' @return A data frame with the MASE of each scenario and index.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # Prepare the data and extract the MASE
#' data <- hindcast_data(list(hc_S01, hc_S02))
#' get_mase(data)
#'
#' # Display as a table
#' summary_table(get_mase(data))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' @family hindcasts functions
#' 
#' @export
get_mase <- function(df_lists) {
  return(df_lists$mase_data)
}

#' Extract parameters data from fitted models
#' 
#' Extracts the model parameter of one or more JABBA fits and combines them 
#' into a single data frame, with one row per scenario and parameter. The 
#' result can be displayed with \code{\link{summary_table}()}.
#' 
#' @details
#' For each fit, the \code{pars} component is converted to a data frame. The 
#' row names become the \code{Indicator} column, and the \code{Scenario} of the 
#' is added as another column. The data frame of all fits are then combined by 
#' rows, which makes it easier to compare the estimates across scenarios.  
#' 
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario). A
#'   single fit is automatically wrapped into a list.
#' 
#' @return A data frame containing parameter estimates for each model, 
#'   including the parameter name and associated scenario.
#' 
#' @return A data frame with one row per scenario and indicator. The firts two 
#'   columns are \code{Scenario} and \code{Indicator}, followed by the columns 
#'   of the \code{pars} component. Returns \code{NULL} if 
#'   \code{lits_fit_models} is \code{NULL} or an empty list.
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
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # A single fit
#' get_pars(fit.S01)
#'
#' # A list of fits (one per scenario)
#' get_pars(list(fit.S01, fit.S02))
#'
#' # Display as a table
#' summary_table(get_pars(list(fit.S01, fit.S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
get_pars <- function(list_fit_models) {
  if (is.null(list_fit_models) || identical(list_fit_models, list())) {
    return(NULL)
  }
  
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
#' Extracts the prior-posterior mean ratio (PPMR) of each parameter and
#' scenario from the output of \code{\link{priors_posteriors_data}()}. The
#' result can be displayed with \code{\link{summary_table}()}.
#'
#' @details
#' The PPMR is the posterior mean divided by the prior mean of each
#' parameter. Values close to 1 indicate that the data did not change the
#' mean of the prior much. The data frame is in wide format, with one row per
#' scenario and one column per parameter.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{priors_posteriors_data}()}, with the element \code{PPMR}.
#'
#' @return A data frame with one row per scenario. The first column is
#'   \code{Scenario}, followed by the PPMR of \code{K}, \code{r} and
#'   \code{psi}.
#'
#' @details
#' The returned data frame is in wide format, with one row per scenario and one 
#' column per indicator. This function is a convenience accessor for extracting 
#' PPMR results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and extract the PPMR
#' data <- priors_posteriors_data(list(fit.S01, fit.S02))
#' get_ppmr(data)
#'
#' # Display as a table
#' summary_table(get_ppmr(data))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' @family priors vs posteriors functions
#'
#' @export
get_ppmr <- function(df_lists) {
  return(df_lists$PPMR)
}

#' Extract PPVR data from priors/posteriors results
#'
#' Extracts the prior-posterior variance ratio (PPVR) of each parameter and
#' scenario from the output of \code{\link{priors_posteriors_data}()}. The
#' result can be displayed with \code{\link{summary_table}()}.
#' 
#' @details
#' The PPVR is the squared coefficient of variation of the posterior divided
#' by that of the prior. Values close to 1 indicate that the data did not
#' change the uncertainty of the prior much, and values below 1 indicate that
#' the posterior is more concentrated than the prior. The data frame is in
#' wide format, with one row per scenario and one column per parameter.
#'
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{priors_posteriors_data}()}, with the element \code{PPVR}.
#'
#' @return A data frame with one row per scenario. The first column is
#'   \code{Scenario}, followed by the PPVR of \code{K}, \code{r} and
#'   \code{psi}.
#'
#' @details
#' The returned data frame is in wide format, with one row per scenario and one 
#' column per indicator. This function is a convenience accessor for extracting 
#' PPVR results for further analysis or visualization.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # Prepare the data and extract the PPVR
#' data <- priors_posteriors_data(list(fit.S01, fit.S02))
#' get_ppvr(data)
#'
#' # Display as a table
#' summary_table(get_ppvr(data))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' @family priors vs posteriors functions
#'
#' @export
get_ppvr <- function(df_lists) {
  return(df_lists$PPVR)
}

#' Extract reference points data from fitted models
#' 
#' Extracts the reference points (refpts) of one or more JABBA fits and 
#' combines them into a single data frame, with one row per scenario and 
#' type of estimate. The result can be displayed with 
#' \code{\link{summary_table}()}. 
#' 
#' @details
#' The \code{refpts} component of each fit is extracted and the data frames are 
#' combined by rows. In the rows where \code{Quant} is \code{"logse"}, the 
#' values of \code{k}, \code{bmsy}, \code{fmsy} and \code{msy} are 
#' exponentiated, so they are shown on the original scale (a multiplicative
#' factor) instead of the log scale. The columns \code{level}, \code{factor} 
#' and \code{quant} are renamed to \code{Scenario}, \code{Factor} and 
#' \code{Quant}.
#' 
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario). A
#'   single fit is automatically wrapped into a list.
#' 
#' @return A data frame with one row per scenario and type of estimate
#'   (\code{Quant}, for example \code{"hat"} for the estimate and
#'   \code{"logse"} for its standard error), with the columns
#'   \code{Factor}, \code{Scenario}, \code{Quant}, \code{k}, \code{bmsy},
#'   \code{fmsy} and \code{msy}. Returns \code{NULL} if
#'   \code{list_fit_models} is \code{NULL} or an empty list.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # A single fit
#' get_refpts(fit.S01)
#'
#' # A list of fits (one per scenario)
#' get_refpts(list(fit.S01, fit.S02))
#'
#' # Display as a table
#' summary_table(get_refpts(list(fit.S01, fit.S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr %>% across bind_rows mutate rename
get_refpts <- function(list_fit_models) {
  if (is.null(list_fit_models) || identical(list_fit_models, list())) {
    return(NULL)
  }
  
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
#' Extracts the retrospective bias (rho) of each indicator and scenario from
#' the output of \code{\link{retrospective_analysis_data}()}, in wide format.
#' The result can be displayed with \code{\link{summary_table}()}.
#' 
#' @details
#' The \code{rho_data} element is reduced to the columns \code{Scenario},
#' \code{Index} and \code{rho}, and pivoted to wide format: one row per
#' scenario and one column per indicator. The rho is the retrospective bias:
#' values close to zero indicate low bias, positive values indicate
#' overestimation and negative values indicate underestimation.
#' 
#' @param df_lists A \code{JAGGdata} list as returned by
#'   \code{\link{retrospective_analysis_data}()}, with the element
#'   \code{rho_data}.
#' 
#' @return A data frame in wide format, with one row per scenario. The first
#'   column is \code{Scenario}, followed by one column of rho values for each
#'   indicator (for example, \code{B}, \code{F}, \code{BBmsy}). Returns
#'   \code{NULL} if \code{rho_data} is not found in \code{df_lists}.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' # Prepare the data and extract rho
#' data <- retrospective_analysis_data(list(hc_S01, hc_S02))
#' get_rho(data)
#'
#' # Display as a table
#' summary_table(get_rho(df))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' @family retrospective analysis functions
#' 
#' @export
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_wider
get_rho <- function(df_lists) {
  temp00 <- df_lists$rho_data 
  if (is.null(temp00)) return(NULL)

  temp01 <- temp00 %>%
    select(Scenario, Index, rho)

  temp02 <- pivot_wider(
    temp01, names_from = "Index", values_from = "rho"
  )
  
  return(temp02)
}

#' Extract summary statistics from fitted models
#' 
#' Extracts the summary statistics of one or more JABBA fits and combines them
#' into a single data frame in wide format, with one row per scenario and one 
#' column per statistic. The result can be displayed with 
#' \code{\link{summary_table}()}.
#' 
#' @details
#' For each fit, the \code{stats} component is converted to a data frame and
#' the \code{Scenario} is added as another column. The data frames of all
#' fits are combined by rows and then pivoted to wide format: the names in
#' the \code{Stastistic} column become the columns, and the \code{Value}
#' entries fill the cells. This makes it easier to compare the statistics
#' across scenarios.
#' 
#' @param list_fit_models Either a single fit returned by
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario). A
#'   single fit is automatically wrapped into a list.
#' 
#' @return A data frame in wide format, with one row per scenario. The first
#'   column is \code{Scenario}, followed by one column for each statistic.
#'   Returns \code{NULL} if \code{list_fit_models} is \code{NULL} or an empty
#'   list.
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input and fit the model for each scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#'
#' # A single fit
#' get_stats(fit.S01)
#'
#' # A list of fits (one per scenario)
#' get_stats(list(fit.S01, fit.S02))
#'
#' # Display as a table
#' summary_table(get_stats(list(fit.S01, fit.S02)))
#' }
#' 
#' @seealso \code{\link{summary_table}}
#' @family extraction functions
#' 
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
get_stats <- function(list_fit_models) {
  if (is.null(list_fit_models) || identical(list_fit_models, list())) {
    return(NULL)
  }
  
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

  temp04 <- as.data.frame(temp03)

  return(temp04)
}