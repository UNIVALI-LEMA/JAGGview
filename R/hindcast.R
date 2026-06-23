#' Prepare hindcast analysis data
#'
#' Processes model outputs to generate data structures for hindcast
#' diagnostics, including observed and predicted values, filtered
#' hindcast points, and accuracy metrics (MASE).
#'
#' @param list_hc_models A list containing retrospective model outputs as 
#' returned by the JABBA function \code{JABBA::hindcast_jabba()}.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{data}{A data frame containing full hindcast time series,
#'   including observed and predicted values.}
#'   \item{hindcast_data_1}{A data frame with selected hindcast points
#'   used for visualization of observed and predicted values.}
#'   \item{hindcast_data_2}{A data frame containing filtered hindcast
#'   trajectories for plotting purposes.}
#'   \item{mase_data}{A data frame containing Mean Absolute Scaled Error
#'   (MASE) metrics for each index and scenario.}
#' }
#'
#' @details
#' The function extracts hindcast runs, formats retrospective labels,
#' filters relevant years and conditions, and computes MASE statistics
#' using \code{JABBA::jbmase}. The output is structured for direct use
#' in visualization functions.
#'
#' @examples
#' \dontrun{
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' df <- hindcast_data(list_hc_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter mutate case_when rename group_by ungroup
hindcast_data <- function(list_hc_models) {
  # ###@> Filtering the expected data...
  # .validate_hcs_input_data(list_hc_models)
  if (.is_hindcast_jabba(list_hc_models)) {
    list_hc_models <- list(list_hc_models)
  }

  ######@> Plot hindcasting...
  hc <- .process_hindcasts(list_hc_models)

  min_year <- as.integer(gsub("-", "", min(hc$Peel))) - 1

  #####@> Extracting data...
  tmp14 <- hc %>%
    rename(retro = Peel) %>%
    mutate(
      retro = fct_relevel(retro, sort(unique(retro), decreasing = TRUE))
    ) %>%
    rename(
      Scenario = level,
      Index = name
    )
  
  tmp15 <- tmp14 %>%
    filter(hindcast == TRUE, year > min_year) %>%
    group_by(retro.peels) %>%
    filter(year == min(year)) %>%
    ungroup()
  
  tmp16 <- .filter_by_condition(tmp14, "retro.peels", "hindcast", "year")

  #####@> MASE analysis...
  mase <- .process_mase(list_hc_models)

  na_index <- mase %>%
    filter(is.na(MASE)) %>%
    pull(unique(Index))
  
  results <- list(
    data = tmp14 %>% filter(!Index %in% na_index),
    hindcast_data_1 = tmp15 %>% filter(!Index %in% na_index),
    hindcast_data_2 = tmp16 %>% filter(!Index %in% na_index),
    mase_data = mase %>% filter(!Index %in% na_index),
    min_year_retro = min_year
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }
  
  return(results)
}

#' Plot hindcast diagnostics
#'
#' Creates a ggplot2-based visualization of hindcast diagnostics, including 
#' observed and predicted values, uncertainty intervals, and model performance 
#' metrics (MASE).
#'
#' @param df_lists A named list as returned by \code{hindcast_data()}.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 4.
#' @param title_x A character string for the x-axis label. Defaults to "Year".
#' @param title_y A character string for the y-axis label. Defaults to "Index".
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#' @param zoom Optional. A boolean value that if \code{TRUE} shows a subplot of 
#'   a zoomed view of the hindcast window. Facets with no data for a given
#'   Scenario and Index combination are left blank. Defaults to \code{FALSE}.
#'
#' @return A ggplot object displaying hindcast trajectories, observed data 
#'   points, uncertainty ribbons, and MASE annotations, faceted by scenario and 
#'   index.
#'
#' @details
#' The plot includes credibility ribbons for reference runs, hindcast
#' trajectories, observed and predicted points, and annotations of MASE values. 
#' Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' df <- hindcast_data(list_hc_models)
#' hindcast_ggplot(df)
#' }
#'
#' @export 
#' @importFrom ggplot2 annotate annotation_custom ggplot geom_ribbon aes 
#' geom_line geom_point geom_text ggplotGrob labs facet_wrap scale_fill_manual 
#' scale_colour_manual scale_y_continuous theme guides guide_legend
#' @importFrom dplyr filter vars
#' @importFrom JABBA ss3col
#' @importFrom ggpp geom_plot
hindcast_ggplot <- function(
  df_lists, text_size = 4, title_x = "Year", title_y = "Index", x_lim = NULL, 
  y_lim = NULL, zoom = FALSE
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  .axis_limit(y_lim)

  .axis_limit(x_lim)

  if (is.null(y_lim)) {
    max_y_val <- .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE)
    min_y_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE)
    y_lim <- c(min_y_val, max_y_val)
  }

  if (is.null(x_lim)) {
    max_x_val <- max(df_lists$data$year)
    min_x_val <- min(df_lists$data$year)
    x_lim <- c(min_x_val, max_x_val)
  }

  x_min_zoom <- x_lim[2] - (x_lim[2] - x_lim[1]) * 0.5
  y_min_zoom <- y_lim[2] - (y_lim[2] - y_lim[1]) * 0.5

  zoom_x_lim <- if(!zoom) x_lim else c(x_lim[1], x_min_zoom)

  pos <- .auto_text_position(
    df_lists$data, 
    "year", 
    "hat.uci", 
    xlim = zoom_x_lim,
    ylim = y_lim
  )

  min_year_hc <- min(df_lists$hindcast_data_2$year) - 1

  max_year_hc <- max(df_lists$hindcast_data_2$year)
  
  p1 <- ggplot() +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0),
        aes(x = year,
            ymin = hat.lci, ymax = hat.uci),
        fill = "gray80") +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0, 
                              year < df_lists$min_year_retro),
                aes(x = year, ymin = hat.lci, ymax = hat.uci),
                fill = "gray30", alpha = 0.5) +
    geom_line(data = filter(df_lists$data, hindcast == FALSE),
              aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = df_lists$hindcast_data_2,
              aes(x = year, y = hat, group = retro.peels),
              linewidth = 1, colour = "white") +
    geom_point(data = filter(df_lists$data, retro.peels == 0, 
                            year < df_lists$min_year_retro),
               aes(x = year, y = obs), pch = 21, size = 4,
               fill = "white") +
    geom_point(data = df_lists$hindcast_data_1, show.legend = FALSE,
               aes(x = year, y = obs, fill = retro),
               pch = 21, size = 4) +
    geom_point(data = df_lists$hindcast_data_1, show.legend = FALSE,
               aes(x = year, y = hat, fill = retro),
               pch = 21, size = 2) +
    geom_text(data = df_lists$mase_data,
              aes(x = pos$x, y = pos$y,
                  label = paste0("MASE = ", round(MASE, 3))), 
                  size = text_size) +
    labs(x = title_x, y = title_y, colour = "") +
    facet_wrap(Scenario ~ Index, ncol = length(unique(df_lists$data$Index)), 
              drop = FALSE) +
    facet_grid(rows = vars(Scenario), cols = vars(Index)) +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    .my_theme() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))

  if (!zoom) {
    return(p1)
  } 

  scenarios <- unique(df_lists$data$Scenario)
  indices <- unique(df_lists$data$Index)

  combos <- expand.grid(
    Scenario = scenarios,
    Index = indices,
    stringsAsFactors = FALSE
  )

  combos$plot <- Map(
    .make_zoom_plot, 
    combos$Scenario, 
    combos$Index,
    MoreArgs = list(
      df = df_lists,
      min_year = min_year_hc,
      max_year = max_year_hc
    )
  )

  combos_valid <- combos %>% filter(!sapply(plot, is.null))

  combos_valid$x <- x_lim[2]
  combos_valid$y <- y_lim[2]

  zoom_ribbon_all <- df_lists$data %>%
    filter(year >= min_year_hc, retro.peels == 0)

  min_y_zoom_ribbon <- floor(min(zoom_ribbon_all$hat.lci) * 10) / 10
  max_y_zoom_ribbon <- ceiling(max(zoom_ribbon_all$hat.uci) * 10) / 10

  min_y_zoom_points_1 <- floor(min(df_lists$hindcast_data_1$obs) * 10) / 10
  max_y_zoom_points_1 <- ceiling(max(df_lists$hindcast_data_1$obs) * 10) / 10

  min_y_zoom_points_2 <- floor(min(df_lists$hindcast_data_2$obs) * 10) / 10
  max_y_zoom_points_2 <- ceiling(max(df_lists$hindcast_data_2$obs) * 10) / 10

  min_y_zoom <- min(min_y_zoom_ribbon, min_y_zoom_points_1, min_y_zoom_points_2)
  max_y_zoom <- max(max_y_zoom_ribbon, max_y_zoom_points_1, max_y_zoom_points_2)

  rect_data <- combos_valid %>%
    select(Scenario, Index) %>%
    mutate(
      xmin = min_year_hc, xmax = max_year_hc, 
      ymin = min_y_zoom,  ymax = max_y_zoom
    )

  p3 <- p1 +
    geom_rect(
      data = rect_data,
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA, colour = "black", linetype = 2, linewidth = 0.5, 
      inherit.aes = FALSE
    ) +
    geom_plot(
      data = combos_valid,
      mapping = aes(x = x, y = y, label = plot),
      vp.width  = 0.45,
      vp.height = 0.45,
      hjust = 1,
      vjust = 1
    )

  p3
}

#' Extract MASE data from hindcast results
#' 
#' Retrieves the data frame containing MASE (Mean Absolute Scaled Errors) values
#' for all indices and scenarios, as returned by \code{hindcast_data()}.
#' 
#' @param df_lists A named list object returned by \code{hindcast_data()}, which 
#'   must contain a component named \code{"mase_data"}.
#' 
#' @return A data frame containing Mean Absolute Scaled Error (MASE) metrics 
#' for each index and scenario.
#' 
#' @details
#' The returned data frame is in wide format, with one row per combination of 
#' Index and Scenario. This function is a convenience acessor for extracting
#' MASE results for further analysis or visualization.
#' 
#' @export
get_mase <- function(df_lists) {
  return(df_lists$mase_data)
}

#' Extract hindcast diagnostics from model outputs
#'
#' Internal helper that extracts hindcast diagnostic data from model outputs 
#' and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing hindcast diagnostics across scenarios.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_hindcasts <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      temp01 <- lapply(
        names(fit),
        function(nm) {
          data <- fit[[nm]]
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          data.frame(
            Peel = peel,
            data$diags
          )
        }
      )
      bind_rows(temp01)
    }
  )
  bind_rows(temp00)
}

#' Filter data based on conditional transitions
#'
#' Internal helper that filters grouped data by identifying the first
#' occurrence of a logical condition and returning selected rows around that 
#' transition.
#'
#' @param df A data frame.
#' @param group_col A character string specifying the grouping column.
#' @param condition_col A character string specifying the logical
#'   condition column.
#' @param year_col A character string specifying the time variable.
#'
#' @return A filtered data frame containing selected observations.
#'
#' @keywords internal
#' @importFrom dplyr group_by across all_of group_modify filter ungroup
.filter_by_condition <- function(df, group_col, condition_col, year_col) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    group_modify(~ {
      data_group <- .x
      first_true_index <- which(data_group[[condition_col]] == TRUE)[1]
      if (!is.na(first_true_index) && first_true_index > 1) {
          target_years <-c(data_group[[year_col]][first_true_index - 1],
            data_group[[year_col]][first_true_index])
          data_group %>% filter(data_group[[year_col]] %in% target_years)
      } else {
          data_group[0,]
      }
    }) %>%
    ungroup()
}

#' Compute MASE diagnostics
#'
#' Internal helper that computes Mean Absolute Scaled Error (MASE) metrics from 
#' model outputs.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing MASE values for each scenario, including 
#' plotting coordinates.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows everything
#' @importFrom JABBA jbmase
.process_mase <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      jbmase(fit) %>% 
        mutate(
          Scenario = fit[[1]]$scenario
        )
    }
  )
  result <- bind_rows(temp00) %>% 
    filter(Index != "joint") %>%
    select(Scenario, everything())
  return(result)
}

#' Create zoom plot for hindcast visualization
#' 
#' Generates a zoomed ggplot for a specific Scenario and Index combination,
#' intended to be embedded as an inset in \code{hindcast_ggplot()}. The plot
#' displays the ribbon, fitted lines, and observed/predicted points restricted
#' to the hindcast window defined by \code{min_year} and \code{max_year}.
#'
#' @param df A \code{JAGGdata} object containing the components \code{data},
#'   \code{hindcast_data_1}, and \code{hindcast_data_2}, as returned by
#'   \code{hindcast_data()}.
#' @param scen A character string specifying the scenario name to filter.
#' @param idx A character string specifying the index name to filter.
#' @param min_year A numeric value indicating the first year of the zoom window
#'   (x-axis left bound).
#' @param max_year A numeric value indicating the last year of the zoom window
#'   (x-axis right bound).
#'
#' @return A \code{ggplot} object representing the zoomed inset plot, or
#'   \code{NULL} if no data is available for the given Scenario and Index
#'   combination.
#'
#' @details
#' This function is called internally by \code{hindcast_ggplot()} via
#' \code{Map()} to produce one inset per Scenario x Index facet. Combinations
#' with no data in \code{hindcast_data_1}, \code{hindcast_data_2}, or the
#' ribbon data are silently skipped by returning \code{NULL}, which prevents
#' empty panels from receiving an inset.
#' 
#' @keywords internal
.make_zoom_plot <- function(df, scen, idx, min_year, max_year) {
  zoom_data <- df$data %>%
    filter(year >= min_year, Scenario == scen, Index == idx)

  zoom_data_ribbon <- zoom_data %>%
    filter(retro.peels == 0)

  hc_data_1 <- df$hindcast_data_1 %>%
    filter(Scenario == scen, Index == idx)

  hc_data_2 <- df$hindcast_data_2 %>%
    filter(Scenario == scen, Index == idx)

  if (nrow(zoom_data_ribbon) == 0 || nrow(hc_data_1) == 0
    || nrow(hc_data_2) == 0) {
    return(NULL)
  }

  ggplot() +
    geom_ribbon(data = zoom_data_ribbon,
      aes(x = year, ymin = hat.lci, ymax = hat.uci), fill = "gray80") +
    geom_line(data = filter(zoom_data, hindcast == FALSE),
      aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = hc_data_2,
      aes(x = year, y = hat, group = retro.peels),
      linewidth = 1, colour = "white") +
    geom_point(data = hc_data_1, show.legend = FALSE,
      aes(x = year, y = obs, fill = retro), pch = 21, size = 4) +
    geom_point(data = hc_data_1, show.legend = FALSE,
      aes(x = year, y = hat, fill = retro), pch = 21, size = 2) +
    labs(x = "Year", y = "Index", colour = "") +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    coord_cartesian(xlim = c(min_year, max_year)) +
    .my_theme() +
    theme(legend.position = "none")
}