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
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
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
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_point geom_text
#' labs facet_wrap scale_fill_manual scale_colour_manual scale_y_continuous
#' theme guides guide_legend
#' @importFrom dplyr filter vars
#' @importFrom JABBA ss3col
hindcast_ggplot <- function(df_lists, text_size = 4, y_lim = NULL) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }

  .axis_limit(y_lim)

  if (is.null(y_lim)) {
    max_val <- .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE)
    min_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE)
    y_lim <- c(min_val, max_val)
  }

  pos <- .auto_text_position(
    df_lists$data, 
    "year", 
    "hat.uci", 
    ylim = y_lim
  )
  
  ggplot() +
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
    labs(x = "Year", y = "Index", colour = "") +
    facet_wrap(Scenario ~ Index, ncol = length(unique(df_lists$data$Index)), 
              drop = FALSE) +
    facet_grid(rows = vars(Scenario), cols = vars(Index)) +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(limits = y_lim) +
    .my_theme() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
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