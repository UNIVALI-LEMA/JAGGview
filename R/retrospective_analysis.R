#' Prepare retrospective analysis data
#'
#' Processes retrospective model outputs to generate time series data, surplus 
#' production curves, and retrospective bias metrics (rho) for multiple 
#' scenarios and indices.
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
#'   \item{rho_data}{A data frame containing retrospective bias estimates (rho) 
#'   for each index and scenario.}
#' }
#'
#' @details
#' The function extracts retrospective runs, filters relevant indices, and 
#' structures the data for visualization. It also computes logical filters for 
#' valid retrospective years and includes surplus production and rho diagnostics.
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
#' Creates a ggplot2-based visualization of retrospective analyses, including 
#' time series of key indices and surplus production curves, along with 
#' retrospective bias (rho) annotations.
#'
#' @param df_lists A named list as returned by
#'   \code{retrospective_analysis_data()}.
#' @param indicator_name A character string specifying the name of the 
#'   indicator to plot. Supported values include "B", "F", "BBmsy", "FFmsy",
#'   "procB", and "MSY".
#' @param n_col An integer value that determines the maximum number of columns
#'   per line. Defaults to 3.
#' @param use_si_suffix A boolean value indicating whether SI suffixes will be 
#'   used, or if FALSE then shows the absolute number, Defaults to FALSE.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 4.
#' @param title_y A character string for the y-axis label. If \code{NULL}, a 
#'   default label is assigned based on \code{indicator_name}.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying retrospective trajectories, credibility 
#' intervals (when applicable), and rho annotations.
#'
#' @details
#' For standard indices, the plot shows time series with credibility ribbons 
#' and retrospective trajectories. For "MSY", the plot displays surplus 
#' production curves as a function of biomass. Results are faceted by scenario.
#'
#' @examples
#' \dontrun{
#' df <- retrospective_analysis_data(list_hc_models)
#' retrospective_analysis_ggplot(df, indicator_name = "B")
#' }
#'
#' @export
#' @importFrom ggplot2 element_text ggplot geom_line aes geom_ribbon geom_text 
#' guides guide_legend facet_wrap scale_colour_manual scale_y_continuous labs 
#' theme  
#' @importFrom JABBA ss3col
retrospective_analysis_ggplot <- function(
  df_lists, indicator_name, n_col = 3, text_size = 4, use_si_suffix = FALSE, 
  title_y = NULL, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if (!indicator_name %in% c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")) {
    stop(paste0(
      "Parameter 'indicator_name' was expecting 'B', 'F', 'BBmsy', 'FFmsy', ",
      "'procB' or 'MSY'."
    ))
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

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
    max_y_val <- .round_to_nearest(max(data_ref$uci, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$lci, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }
    if (is.null(x_lim)) {
      max_x_val <- max(max(data_ref$Year), max(data_var$Year))
      min_x_val <- min(min(data_ref$Year), min(data_var$Year))
    }
    pos <- .auto_text_position(
      data_list = data_ref,
      col_x = "Year",
      col_y = "uci",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.15
    )
  } else {
    max_y_val <- .round_to_nearest(max(data_ref$SP, na.rm = TRUE), TRUE, 1.1)
    if (is.null(y_lim)) {
      min_y_val <- .round_to_nearest(min(data_ref$SP, na.rm = TRUE), FALSE, 1.1)
      y_lim <- c(min_y_val, max_y_val)
    }

    if (is.null(x_lim)) {
      max_x_val <- max(max(data_ref$SB_i), max(data_var$SB_i))
      min_x_val <- min(min(data_ref$SB_i), min(data_var$SB_i))
      x_lim <- c(min_x_val, max_x_val)
    }
    
    pos <- .auto_text_position(
      data_list = data_lines,
      col_x = "SB_i",
      col_y = "SP",
      xlim = x_lim,
      ylim = y_lim,
      margin = 0.2
    )
    max_x_val <- .round_to_nearest(max(data_ref$SB_i, na.rm = TRUE), TRUE, 1.1)
    x_decimals <- ifelse(x_lim[2] > 10, 0, 1)
    x_labels <- if (use_si_suffix) {
      function(x) .international_system_prefixes(x)
    } else {
      function(x) .format_number(x, decimals = x_decimals)
    }
  }

  y_decimals <- ifelse(y_lim[2] > 10, 0, 1)

  y_labels <- if (use_si_suffix) {
    function(x) .international_system_prefixes(x)
  } else {
    function(x) .format_number(x, decimals = y_decimals)
  }
  
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
  
  p <- p +
    geom_text(
      data = rho_var,
      aes(x = pos$x, y = pos$y,
        label = paste0("rho == ", round(rho, 3))), 
        parse = TRUE,
        size = text_size
    ) +
    facet_wrap(~Scenario, ncol = n_col, scales = "fixed") +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), labels = y_labels) +
    coord_cartesian(xlim = x_lim, ylim = y_lim)

  if (indicator_name == "MSY") {
    p <- p +
      scale_x_continuous(labels = x_labels)
  }
  
  p <- p +
    labs(x = title_x, y = title_y, colour = "") +
    .my_theme() +
    theme(
      legend.position = "bottom",
      legend.justification = c(0, 1),
      legend.text = element_text(size = 12)
    ) +
    guides(colour = guide_legend(nrow = 1))
  p
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
#' @export
get_rho <- function(df_lists) {
  return(df_lists$rho_data)
}

#' Extract retrospective time series
#'
#' Internal helper that extracts time series data from retrospective model runs 
#' and reshapes them into a combined data frame.
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
#' Internal helper that extracts surplus production function outputs from 
#' retrospective model runs.
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
#' Internal helper that computes retrospective bias statistics (rho) using 
#' JABBA outputs.
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
        .extract_rhos(
          .jbplot_retro2(hc, verbose = FALSE, rhoout = TRUE, plot = FALSE)
        )
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

#' .jbplot_retro2() to plot retrospective pattern
#'
#' Plots retrospective pattern of B, F, BBmsy, FFmsy, BB0 and SP #'
#' @param hc output list from hindast_jabba()
#' @param type  single plot option c("B","F","BBmsy","FFmsy","BB0","SP")
#' @param forecast  includes retrospective forecasting if TRUE
#' @param ylabs yaxis labels for quants
#' @param add  add to multi plot if TRUE
#' @param output.dir directory to save plots
#' @param as.png save as png file of TRUE
#' @param single.plots if TRUE plot invidual fits else make multiplot
#' @param width plot width
#' @param height plot hight
#' @param xlim  allows to "zoom-in" requires speficiation Xlim=c(first.yr,last.yr)
#' @param cols option to add colour palette 
#' @param legend.loc location of legend
#' @param verbose if FALSE be silent
#' @param rhoout if TRUE, produces the rho data.frame as output 
#' @param plot if TRUE, produces the plots
#' @return Mohn's rho statistic for several quantaties
#' @keywords internal
#' @examples 
#' \dontrun{
#' if (requireNamespace("JABBA", quietly = TRUE)) {
#'   data("iccat", package = "JABBA")
#'   bet <- iccat$bet
#'   
#'   .jbplot_retro2(bet)
#' }
#' }
.jbplot_retro2 <- function (hc, type = c("B", "F", "BBmsy", "FFmsy", "procB", "SP"), 
    forecast = FALSE, ylabs = NULL, add = F, output.dir = getwd(), 
    as.png = FALSE, single.plots = add, width = NULL, height = NULL, 
    xlim = NULL, cols = NULL, legend.loc = "topright", verbose = TRUE,
    rhoout = TRUE, plot = TRUE) 
{
    hc.ls = hc
    peels = as.numeric(do.call(c, lapply(hc.ls, function(x) {
        x$diags$retro.peels[1]
    })))
    Ref = hc.ls[[1]]
    hc = list(scenario = Ref$scenario, yr = Ref$yr, catch = Ref$catch, 
        peels = NULL, timeseries = NULL, refpts = NULL, pfunc = NULL, 
        diags = NULL, settings = Ref$settings)
    for (i in 1:length(peels)) {
        hc.ls[[i]]$pfunc$level = peels[i]
        hc.ls[[i]]$refpts$level = peels[i]
        hc$timeseries$mu = rbind(hc$timeseries$mu, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "mu", 
            ]))
        hc$timeseries$lci = rbind(hc$timeseries$lci, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "lci", 
            ]))
        hc$timeseries$uci = rbind(hc$timeseries$uci, data.frame(factor = hc.ls[[i]]$diags[1, 
            1], level = peels[i], hc.ls[[i]]$timeseries[, "uci", 
            ]))
        hc$diags = rbind(hc$diags, hc.ls[[i]]$diags)
        hc$refpts = rbind(hc$refpts, hc.ls[[i]]$refpts[1, ])
        hc$pfunc = rbind(hc$pfunc, hc.ls[[i]]$pfunc)
    }
    if (verbose) 
        cat(paste0("\n", "><> jbplot_retro() - retrospective analysis <><", 
            "\n"))
    if (add) 
        single.plots = TRUE
    if (single.plots == F) 
        type = c("B", "F", "BBmsy", "FFmsy", "procB", "SP")
    if (is.null(ylabs)) 
        ylabs = c(paste("Biomass", hc$settings$catch.metric), 
            "Fishing mortality F", expression(B/B[MSY]), expression(F/F[MSY]), 
            expression(B/B[0]), "Process Deviations", paste("Surplus Production", 
                hc$settings$catch.metric))
    retros = unique(peels)
    runs = hc$timeseries$mu$level
    years = hc$yr
    nyrs = length(years)
    if (is.null(cols)) 
        cols = c("black", ss3col(length(peels) - 1))
    if (is.null(xlim)) {
        xlim = range(years)
    }
    FRP.rho = c("B", "F", "Bmsy", "Fmsy", "procB", "MSY")
    rho = data.frame(mat.or.vec(length(retros) - 1, length(FRP.rho)))
    colnames(rho) = FRP.rho
    fcrho = rho
    for (k in 1:length(type)) {
        j = which(c("B", "F", "BBmsy", "FFmsy", "BB0", "procB", 
            "SP") %in% type[k])
        if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
            y = hc$timeseries$mu[, j + 2]
            ref = hc$timeseries$mu[runs %in% retros[1], j + 
              2]
            ylc = hc$timeseries$lci[runs %in% retros[1], 
              j + 2]
            yuc = hc$timeseries$uci[runs %in% retros[1], 
              j + 2]
            for (i in 1:length(retros)) {
              if (i > 1) {
                rho[i - 1, k] = (y[runs %in% retros[i]][(nyrs - 
                  retros[i])] - ref[(nyrs - retros[i])])/ref[(nyrs - 
                  retros[i])]
                fcrho[i - 1, k] = (y[runs %in% retros[i]][(nyrs + 
                  1 - retros[i])] - ref[(nyrs + 1 - retros[i])])/ref[(nyrs + 
                  1 - retros[i])]
                if (type[k] == "procB") {
                  rho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs - 
                    retros[i])]) - exp(ref[(nyrs - retros[i])]))/exp(ref[(nyrs - 
                    retros[i])])
                  if (single.plots == TRUE) {
                    fcrho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs + 
                    1 - retros[i])]) - exp(ref[(nyrs + 1 - 
                    retros[i])]))/exp(ref[(nyrs + 1 - retros[i])])
                  }
                  else {
                    fcrho[i - 1, k] = (exp(y[runs %in% retros[i]][(nyrs - 
                      retros[i])]) - exp(ref[(nyrs + 1 - retros[i])]))/exp(ref[(nyrs + 
                      1 - retros[i])])
                  }
                }
              }
            }
        }
        else {
            for (i in 1:length(retros)) {
              if (i > 1) {
                rho[i - 1, 6] = (hc$refpts$msy[hc$refpts$level == 
                  retros[i]] - hc$refpts$msy[hc$refpts$level == 
                  retros[1]])/hc$refpts$msy[hc$refpts$level == 
                  retros[1]]
                fcrho[i - 1, 6] = NA
              }
            }
        }
    }
    if (plot) {
        if (single.plots == TRUE) {
            if (is.null(width)) 
                width = 5
            if (is.null(height)) 
                height = 3.5
            for (k in 1:length(type)) {
                Par = list(mfrow = c(1, 1), mar = c(3.5, 3.5, 0.5, 
                  0.1), mgp = c(2, 0.5, 0), tck = -0.02, cex = 0.8)
                if (as.png == TRUE) {
                    png(filename = paste0(output.dir, "/Retro", hc$scenario, 
                      "_", type[k], ".png"), width = width, height = height, 
                      res = 200, units = "in")
                }
                if (as.png == TRUE | add == FALSE) 
                    par(Par)
                if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
                    if (type[k] == "procB") 
                      ylim = c(-max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]), max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    if (!type[k] == "procB") 
                      ylim = c(0, max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    plot(years, years, type = "n", ylim = ylim, ylab = ifelse(length(ylabs) > 
                      1, ylabs[j], ylabs), xlab = "Year", xlim = xlim)
                    polygon(c(years, rev(years)), c(ylc, rev(yuc)), 
                      col = "grey", border = "grey")
                    for (i in 1:length(retros)) {
                      lines(years[1:(nyrs - retros[i])], y[runs %in% 
                        retros[i]][1:(nyrs - retros[i])], col = cols[i], 
                        lwd = ifelse(i == 1, 2, 1.5), lty = 1)
                      if (forecast) {
                        lines(years[(nyrs - retros[i]):(nyrs + 1 - 
                          retros[i])], y[runs %in% retros[i]][(nyrs - 
                          retros[i]):(nyrs + 1 - retros[i])], col = cols[i], 
                          lwd = 1, lty = 2)
                        points(years[(nyrs + 1 - retros[i])], y[runs %in% 
                          retros[i]][(nyrs + 1 - retros[i])], pch = 16, 
                          col = cols[i], cex = 0.8)
                      }
                    }
                    if (type[k] %in% c("BBmsy", "FFmsy")) 
                      abline(h = 1, lty = 2)
                    if (type[k] %in% c("procB")) 
                      abline(h = 0, lty = 2)
                }
                else {
                    plot(years, years, type = "n", ylim = c(0, max(hc$pfunc$SP * 
                    1.12)), xlim = c(0, max(hc$pfunc$SB_i)), ylab = ifelse(length(ylabs) > 
                    1, ylabs[j], ylabs), xlab = ylabs[1])
                    for (i in 1:length(retros)) {
                      lines(hc$pfunc$SB_i[hc$pfunc$level %in% retros[i]], 
                        hc$pfunc$SP[hc$pfunc$level %in% retros[i]], 
                        col = cols[i], lwd = ifelse(i == 1, 2, 1.5), 
                        lty = 1)
                      points(mean(hc$pfunc$SB_i[hc$pfunc$level %in% 
                        retros[i]][hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]] == max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]])]), max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]]), col = cols[i], pch = 16, cex = 1.2)
                    }
                }
                if (single.plots == TRUE | k == 1) 
                    legend(legend.loc, paste(years[nyrs - retros]), 
                      col = cols, bty = "n", cex = 0.7, pt.cex = 0.7, 
                      lwd = c(2, rep(1.5, length(retros))))
                if (!forecast) 
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
                if (forecast) 
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2)) ~ "(" ~ .(round(mean(fcrho[, k]), 
                      2)) ~ ")"), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
            }
        }
        else {
            if (is.null(width)) 
                width = 7
            if (is.null(height)) 
                height = 8
            Par = list(mfrow = c(3, 2), mai = c(0.45, 0.49, 0.1, 
                0.15), omi = c(0.15, 0.15, 0.1, 0) + 0.1, mgp = c(2, 
                0.5, 0), tck = -0.02, cex = 0.8)
            if (as.png == TRUE) {
                png(filename = paste0(output.dir, "/Retro_", hc$scenario, 
                    ".png"), width = width, height = height, res = 200, 
                    units = "in")
            }
            par(Par)
            for (k in 1:length(type)) {
                if (type[k] %in% c("B", "F", "BBmsy", "FFmsy", "procB")) {
                    if (type[k] == "procB") 
                      ylim = c(-max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]), max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    if (!type[k] == "procB") 
                      ylim = c(0, max(y[years >= xlim[1] & years <= 
                        xlim[2]], yuc[years >= xlim[1] & years <= 
                        xlim[2]]))
                    plot(years, years, type = "n", ylim = ylim, ylab = ylabs[j], 
                      xlab = "Year", xlim = xlim)
                    polygon(c(years, rev(years)), c(ylc, rev(yuc)), 
                      col = "grey", border = "grey")
                    for (i in 1:length(retros)) {
                      lines(years[1:(nyrs - retros[i])], y[runs %in% 
                        retros[i]][1:(nyrs - retros[i])], col = cols[i], 
                        lwd = ifelse(i == 1, 2, 1.5), lty = 1)
                      if (forecast) {
                        lines(years[(nyrs - retros[i]):(nyrs + 1 - 
                          retros[i])], y[runs %in% retros[i]][(nyrs - 
                          retros[i]):(nyrs + 1 - retros[i])], col = cols[i], 
                          lwd = 1, lty = 2)
                        points(years[(nyrs + 1 - retros[i])], y[runs %in% 
                          retros[i]][(nyrs + 1 - retros[i])], pch = 16, 
                          col = cols[i], cex = 0.8)
                      }
                    }
                    if (type[k] %in% c("BBmsy", "FFmsy")) 
                      abline(h = 1, lty = 2)
                    if (type[k] %in% c("procB")) 
                      abline(h = 0, lty = 2)
                    if (single.plots == TRUE | k == 1) 
                      legend(legend.loc, paste(years[nyrs - retros]), 
                        col = cols, bty = "n", cex = 0.7, pt.cex = 0.7, 
                        lwd = c(2, rep(1.5, length(retros))))
                    if (!forecast) 
                      legend("top", legend = bquote(rho == .(round(mean(rho[, 
                        k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                        cex = 0.8)
                    if (forecast) 
                      legend("top", legend = bquote(rho == .(round(mean(rho[, 
                        k]), 2)) ~ "(" ~ .(round(mean(fcrho[, k]), 
                        2)) ~ ")"), bty = "n", x.intersp = -0.2, 
                        y.intersp = -0.3, cex = 0.8)
                }
                else {
                    plot(years, years, type = "n", ylim = c(0, max(hc$pfunc$SP * 
                      1.15)), xlim = c(0, max(hc$pfunc$SB_i)), ylab = ylabs[j], 
                      xlab = ylabs[1])
                    for (i in 1:length(retros)) {
                      lines(hc$pfunc$SB_i[hc$pfunc$level %in% retros[i]], 
                        hc$pfunc$SP[hc$pfunc$level %in% retros[i]], 
                        col = cols[i], lwd = ifelse(i == 1, 2, 1.5), 
                        lty = 1)
                      points(mean(hc$pfunc$SB_i[hc$pfunc$level %in% 
                        retros[i]][hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]] == max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]])]), max(hc$pfunc$SP[hc$pfunc$level %in% 
                        retros[i]]), col = cols[i], pch = 16, cex = 1.2)
                    }
                    legend("top", legend = bquote(rho == .(round(mean(rho[, 
                      k]), 2))), bty = "n", x.intersp = -0.2, y.intersp = -0.3, 
                      cex = 0.8)
                }
            }
        }
        if (as.png == TRUE) 
            dev.off()
    }
    rho = rbind(rho, apply(rho, 2, mean))
    rownames(rho) = c(rev(years)[retros[-1]], "rho.mu")
    fcrho = rbind(fcrho, apply(fcrho, 2, mean))
    rownames(fcrho) = c(rev(years)[retros[-1]], "forecastrho.mu")
    if (forecast) {
        out = list()
        out$Mohns.rho = rho
        out$Forecast.rho = fcrho
    }
    else {
        out = rho
    }
    if (rhoout) 
        return(out)
}
