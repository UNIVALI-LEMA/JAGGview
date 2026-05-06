#' Prepare retrospective analysis data
#'
#' Processes retrospective model outputs to generate time series data,
#' surplus production curves, and retrospective bias metrics (rho)
#' for multiple scenarios and indices.
#'
#' @param list_hc_models A list containing retrospective model outputs as 
#' returned by the JABBA function \code{JABBA::hindcast_jabba()}.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{data}{A data frame containing time series for key indices
#'   (e.g., B, F, B/Bmsy, F/Fmsy, and process error).}
#'   \item{surplus_data}{A data frame containing surplus production
#'   (MSY-related) data.}
#'   \item{rho_data}{A data frame containing retrospective bias
#'   estimates (rho) for each index and scenario.}
#' }
#'
#' @details
#' The function extracts retrospective runs, filters relevant indices,
#' and structures the data for visualization. It also computes logical
#' filters for valid retrospective years and includes surplus production
#' and rho diagnostics.
#'
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' df <- retrospective_analysis_data(list_hc_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter mutate
retrospective_analysis_data <- function(list_hc_models) {
  # ###@> Filtering the expected data...
  # .validate_hcs_input_data(list_hc_models)
  if (.is_hindcast_jabba(list_hc_models)) {
    list_hc_models <- list(list_hc_models)
  }

  labels_index <- c(
    "B"      = "Biomass",
    "F"      = "Fishing Mortality",
    "BBmsy"  = "B/Bmsy",
    "FFmsy"  = "F/Fmsy",
    "procB"  = "Process Error on log(Biomass)"
  )

  #####@> Extracting values...
  tmp17 <- .process_retro(list_hc_models) %>%
    filter(Index %in% c("B", "F", "BBmsy", "FFmsy", "procB")) %>%
    mutate(
      Index2 = labels_index[Index]
    ) %>%
    mutate(
      id = fct_relevel(id, sort(unique(id), decreasing = TRUE))
    ) %>%
    mutate(
      # This version id_num when id == "Ref" generate NA warning
      # id_num = as.integer(gsub("-", "", id)),
      id_num = {
        out <- rep(NA_integer_, length(id))
        idx <- grepl("^-\\d+$", id)
        out[idx] <- as.integer(sub("-", "", id[idx]))
        out
      },
      teste = ifelse(
        id == "Ref",
        TRUE,
        Year < id_num
      )
    ) %>%
    select(-id_num)

  tmp18 <- .process_pfunc(list_hc_models) %>%
    mutate(
      Index = "MSY",
      Index2 = "Surplus Production"
    ) %>%
      mutate(
        id = fct_relevel(id, sort(unique(id), decreasing = TRUE))
    )

  # #####@> Extracting rhos...
  temp02 <- .rho_retro(list_hc_models)

  results <- list(
    data = tmp17,
    surplus_data = tmp18,
    rho_data = temp02
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Plot retrospective analysis results
#'
#' Creates a ggplot2-based visualization of retrospective analyses,
#' including time series of key indices and surplus production curves,
#' along with retrospective bias (rho) annotations.
#'
#' @param df_lists A named list as returned by
#'   \code{retrospective_analysis_data()}.
#' @param indicator_name A character string specifying the name of the 
#'   indicator to plot. Supported values include "B", "F", "BBmsy", "FFmsy",
#'   "procB", and "MSY".
#' @param title_y A character string for the y-axis label. If \code{NULL},
#'   a default label is assigned based on \code{indicator_name}.
#'
#' @return A ggplot object displaying retrospective trajectories,
#'   confidence intervals (when applicable), and rho annotations.
#'
#' @details
#' For standard indices, the plot shows time series with confidence
#' ribbons and retrospective trajectories. For "MSY", the plot displays
#' surplus production curves as a function of biomass. Results are
#' faceted by scenario.
#'
#' @examples
#' \dontrun{
#' df <- retrospective_analysis_data(list_hc_models)
#' retrospective_analysis_ggplot(df, indicator_name = "B")
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_line aes geom_ribbon geom_text facet_wrap
#' scale_colour_manual scale_y_continuous labs theme element_text
#' @importFrom JABBA ss3col
retrospective_analysis_ggplot <- function(df_lists, indicator_name, title_y = NULL) {
  if(!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if(!indicator_name %in% c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")) {
    stop("Parameter 'indicator_name' was expecting 'B', 'F', 'BBmsy', 'FFmsy', 'procB' or 'MSY'.")
  }

  if (indicator_name != "MSY") {
    data <- df_lists$data
    title_x <- "Year"
  } else {
    data <- df_lists$surplus_data
    title_x <- "Biomass (t)"
  }

  labels_y <- list(
    B = "Biomass (t)",
    F = "Fishing Mortality (F)",
    BBmsy = expression(B/B[MSY]),
    FFmsy = expression(F/F[MSY]),
    procB = "Process error on log(Biomass)",
    MSY = "Surplus Production (t)"
  )

  if (is.null(title_y)) {
    title_y <- labels_y[[indicator_name]]
  }
  
  rho_data <- df_lists$rho_data
  data_var <- data[data$Index == indicator_name, ]
  
  data_ref   <- data_var[data_var$id == "Ref", ]
  if (indicator_name != "MSY") {
    data_lines <- data_var[data_var$teste == TRUE, ]
  } else {
    data_lines <- data_var
  }
  rho_var <- rho_data[rho_data$Index == indicator_name, ]
  
  if (indicator_name != "MSY") {
    max_val <- .round_to_nearest(max(data_var$uci, na.rm = TRUE), TRUE, 1.1)
    min_val <- .round_to_nearest(min(data_var$lci, na.rm = TRUE), FALSE, 1.1)
    ylim <- c(min_val, max_val)
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      ylim = ylim
    )
  } else {
    max_val <- .round_to_nearest(max(data_var$SP, na.rm = TRUE), TRUE, 1.1)
    min_val <- .round_to_nearest(min(data_var$SP, na.rm = TRUE), FALSE, 1.1)
    ylim <- c(min_val, max_val)
    pos <- .auto_text_position(
      data_list = data_lines,
      col_x = "SB_i",
      col_y = "SP",
      ylim = ylim
    )
  }

  y_decimals <- ifelse(max_val > 10, 0, 1)
  
  p <- ggplot()
  
  if (indicator_name != "MSY") {
    p <- p +
      geom_ribbon(
        data = data_ref,
        aes(x = Year, ymin = lci, ymax = uci),
        fill = "gray80"
      ) +
      geom_line(
        data = data_lines,
        aes(x = Year, y = mu, colour = id, group = id),
        linewidth = 1
      )
    if (indicator_name %in% c("BBmsy", "FFmsy")) {
      p <- p +
        geom_hline(yintercept = 1, linetype = "longdash")
    } 
    else if (indicator_name == "procB") {
      p <- p +
        geom_hline(yintercept = 0, linetype = "longdash")
    }  
  } 
  else {
    data_lines <- data_lines[!is.na(data_lines$SB_i) & !is.na(data_lines$SP), ]
    
    p <- p +
      geom_line(
        data = data_lines,
        aes(x = SB_i, y = SP, colour = id, group = id),
        linewidth = 1
      )
  }
  
  p +
    geom_text(
      data = rho_var,
      aes(x = pos$x, y = pos$y,
        label = paste0("rho == ", round(rho, 3))), 
        parse = TRUE
    ) +
    facet_wrap(~Scenario, ncol = 3, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(
      expand = c(0, 0), 
      limits = ylim, 
      labels = function(x) .format_number(x, decimals = y_decimals)
    ) +
    labs(x = title_x, y = title_y, colour = "") +
    .my_theme() +
    theme(
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    )
}

#' Extract rho data from retrospective analysis results
#' 
#' Retrieves the data frame containing rho (retrospective bias metrics) values
#' for all indices and scenarios, as returned by \code{hindcast_data()}.
#' 
#' @param df_lists A named list object returned by \code{hindcast_data()}, which 
#'   must contain a component named \code{"rho_data"}.
#' 
#' @return A data frame where each row represents a combination of scenario and
#'   index, typically including the following columns:
#' \describe{
#'   \item{Scenario}{Scenario identifier}
#'   \item{Index}{Short name of the indicator (e.g., \code{B}, \code{F}, \code{BBmsy})}
#'   \item{Index2}{Descriptive name of the indicator}
#'   \item{rho}{Numeric value representing retrospective bias for the given index}
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
#' @export
get_rho <- function(df_lists) {
  return(df_lists$rho_data)
}

#' Extract retrospective time series
#'
#' Internal helper that extracts time series data from retrospective
#' model runs and reshapes them into a combined data frame.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing time series across scenarios and runs.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          cbind.data.frame(
            id = peel,
            Scenario = hc[[nm]]$scenario,
            .array_to_dataframe(hc[[nm]]$timeseries)
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract surplus production data
#'
#' Internal helper that extracts surplus production function outputs
#' from retrospective model runs.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing surplus production data.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_pfunc <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          cbind.data.frame(
            id = peel,
            Scenario = hc[[nm]]$scenario,
            hc[[nm]]$pfunc
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)

  return(result)
}

#' Extract retrospective bias (rho) values
#'
#' Internal helper that computes retrospective bias statistics (rho)
#' using JABBA outputs.
#'
#' @param hc_list A list of retrospective model outputs.
#'
#' @return A data frame containing rho values by index and scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom JABBA jbplot_retro
.rho_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      cbind.data.frame(
        Scenario = names(hc[1]),
        .extract_rhos(jbplot_retro(hc, as.png = FALSE))
      )
    }
  )
  result <- bind_rows(temp00)
}

#' Format rho values into a data frame
#'
#' Internal helper that converts rho outputs into a structured
#' data frame with index labels.
#'
#' @param rho A matrix or data frame containing rho values.
#'
#' @return A data frame with indices and corresponding rho values.
#'
#' @keywords internal
.extract_rhos <- function(rho) {
  vec01 <- as.numeric(rho[nrow(rho),])
  vec02 <- c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")
  vec03 <- c(
    "Biomass", "Fishing Mortality", "B/Bmsy", "F/Fmsy",
    "Process Error on log(Biomass)", "Surplus Production"
  )
  result <- data.frame("Index" = vec02, "Index2" = vec03, "rho" = vec01)
  return(result)
}