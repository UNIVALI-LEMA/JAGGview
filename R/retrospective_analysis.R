#' Prepare retrospective analysis data
#'
#' Processes retrospective model outputs to generate time series data,
#' surplus production curves, and retrospective bias metrics (rho)
#' for multiple scenarios and indices.
#'
#' @param hc_raw_data A list containing retrospective model outputs as 
#' returned by the JABBA function \code{JABBA::hindcast_jabba()}
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
#' hc_raw_data <- list(hc_S01, hc_S02)
#' df <- retrospective_analysis_data(hc_raw_data)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter mutate
retrospective_analysis_data <- function(hc_raw_data) {
  #####@> Extracting values...
  tmp17 <- .process_retro(hc_raw_data) %>%
    filter(Index %in% c("B", "F", "BBmsy", "FFmsy", "procB")) %>%
    mutate(
      Index2 = ifelse(
        Index == "B", 
        "Biomass",
        ifelse(
          Index == "F", 
          "Fishing Mortality",
          ifelse(
            Index == "BBmsy", 
            "B/Bmsy",
            ifelse(
              Index == "FFmsy", 
              "F/Fmsy",
              "Process Error on log(Biomass)"
            )
          )
        )
      )
    ) %>%
    mutate(
      id = factor(
        id,
        c(
          "Ref", "-2023", "-2022", "-2021", "-2020", 
          "-2019", "-2018", "-2017", "-2016"
        )
      )
    ) %>%
    mutate(
      teste = ifelse(
        id == "Ref", TRUE,
        ifelse(
          id == "-2023" & Year == 2023, FALSE,
          ifelse(
            id == "-2022" & Year >= 2022, FALSE,
            ifelse(
              id == "-2021" & Year >= 2021, FALSE,
              ifelse(
                id == "-2020" & Year >= 2020, FALSE,
                ifelse(
                  id == "-2019" & Year >= 2019, FALSE,
                  ifelse(
                    id == "-2018" & Year >= 2018, FALSE,
                    ifelse(
                      id == "-2017" & Year >= 2017, FALSE,
                      ifelse(
                        id == "-2016" & Year >= 2016, FALSE, 
                        TRUE
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  tmp18 <- .process_pfunc(hc_raw_data) %>%
    mutate(
      Index = "MSY",
      Index2 = "Surplus Production"
    ) %>%
      mutate(
        id = factor(
          id, 
          levels = c(
            "Ref", "-2023", "-2022", "-2021", "-2020", 
            "-2019", "-2018", "-2017", "-2016"
          )
        )
    )

  # #####@> Extracting rhos...
  temp02 <- .rho_retro(hc_raw_data) %>% mutate(x = 2010)

  list(
    data = tmp17,
    surplus_data = tmp18,
    rho_data = temp02
  )
}

#' Plot retrospective analysis results
#'
#' Creates a ggplot2-based visualization of retrospective analyses,
#' including time series of key indices and surplus production curves,
#' along with retrospective bias (rho) annotations.
#'
#' @param df_lists A named list as returned by
#'   \code{retrospective_analysis_data()}.
#' @param var A character string specifying the variable to plot.
#'   Supported values include "B", "F", "BBmsy", "FFmsy",
#'   "procB", and "MSY".
#' @param title_y A character string for the y-axis label. If \code{NULL},
#'   a default label is assigned based on \code{var}.
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
#' df <- retrospective_analysis_data(hc_raw_data)
#' retrospective_analysis_ggplot(df, var = "B")
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_line aes geom_ribbon geom_text facet_wrap
#' scale_colour_manual scale_y_continuous labs theme element_text
#' @importFrom JABBA ss3col
retrospective_analysis_ggplot <- function(df_lists, var, title_y = NULL) {

  if (var != "MSY") {
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

  title_y <- labels_y[[var]]

if (is.null(title_y)) title_y <- var
  
  rho_data <- df_lists$rho_data
  data_var <- data[data$Index == var, ]
  
  data_ref   <- data_var[data_var$id == "Ref", ]
  if (var != "MSY") {
    data_lines <- data_var[data_var$teste == TRUE, ]
  } else {
    data_lines <- data_var
  }
  rho_var    <- rho_data[rho_data$Index == var, ]
  
  if (var == "MSY") {
    max_val <- .round_up_to_nearest(max(data_var$SP, na.rm = TRUE))
    min_val <- 0
  } else {
    max_val <- .round_up_to_nearest(max(data_var$uci, na.rm = TRUE))
    min_val <- .round_up_to_nearest(min(data_var$lci, na.rm = TRUE))
    
    if (min_val > 0) min_val <- 0
  }
  
  p <- ggplot()
  
  if (var != "MSY") {
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
  } else {
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
      aes(x = Inf, y = Inf, label = paste0("rho == ", round(rho, 3))),
      hjust = 1.5, vjust = 2, parse = TRUE
    ) +
    facet_wrap(~Scenario, ncol = 3, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_val, max_val)) +
    labs(x = title_x, y = title_y, colour = "") +
    .my_theme() +
    theme(
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    )
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

#' Round values to a convenient upper bound
#'
#' Internal helper that rounds a numeric value up to the nearest
#' order of magnitude, useful for defining plot axis limits.
#'
#' @param value A numeric value.
#'
#' @return A rounded numeric value.
#'
#' @keywords internal
.round_up_to_nearest <- function(value) {
  magnitude <- 10^(floor(log10(value)))
  rounded_value <- ceiling(value / magnitude) * magnitude
  return(rounded_value)
}