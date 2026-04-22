#' Prepare hindcast analysis data
#'
#' Processes model outputs to generate data structures for hindcast
#' diagnostics, including observed and predicted values, filtered
#' hindcast points, and accuracy metrics (MASE).
#'
#' @param hc_raw_data A list containing retrospective model outputs as 
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
#'   (MASE) metrics for each scenario.}
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
#' df <- hindcast_data(hc_raw_data)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter mutate case_when rename group_by ungroup
hindcast_data <- function(hc_raw_data) {
  ###@> Filtering the expected data...
  .validate_hcs_input_data(hc_raw_data)

  ######@> Plot hindcasting...
  hc <- .process_hindcasts(hc_raw_data)

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
    filter(hindcast == TRUE) %>%
    filter(year > min_year) %>%
    group_by(retro.peels) %>%
    filter(year == min(year)) %>%
    ungroup() %>%
    data.frame
  
  tmp16 <- .filter_by_condition(tmp14, "retro.peels", "hindcast", "year")

  #####@> MASE analysis...
  mase <- .process_mase(hc_raw_data)

  na_index <- mase %>%
    filter(is.na(MASE)) %>%
    pull(Index)

  na_index <- unique(na_index)

  list(
    data = tmp14 %>% filter(!Index %in% na_index),
    hindcast_data_1 = tmp15 %>% filter(!Index %in% na_index),
    hindcast_data_2 = tmp16 %>% filter(!Index %in% na_index),
    mase_data = mase %>% filter(!Index %in% na_index),
    min_year_retro = min_year
  )
}

#' Plot hindcast diagnostics
#'
#' Creates a ggplot2-based visualization of hindcast diagnostics,
#' including observed and predicted values, uncertainty intervals,
#' and model performance metrics (MASE).
#'
#' @param df_lists A named list as returned by
#'   \code{hindcast_data()}.
#'
#' @return A ggplot object displaying hindcast trajectories, observed
#'   data points, uncertainty ribbons, and MASE annotations, faceted
#'   by scenario and index.
#'
#' @details
#' The plot includes confidence ribbons for reference runs, hindcast
#' trajectories, observed and predicted points, and annotations of
#' MASE values. Results are faceted by scenario and index.
#'
#' @examples
#' \dontrun{
#' df <- hindcast_data(hc_raw_data)
#' hindcast_ggplot(df)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_point geom_text
#' labs facet_wrap scale_fill_manual scale_colour_manual scale_y_continuous
#' theme guides guide_legend
#' @importFrom dplyr filter vars
#' @importFrom JABBA ss3col
hindcast_ggplot <- function(df_lists) {
  max_val <- .round_to_nearest(max(df_lists$data$hat.uci, na.rm = TRUE), TRUE)
  min_val <- .round_to_nearest(min(df_lists$data$hat.lci, na.rm = TRUE), FALSE)
  
  ggplot() +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0),
        aes(x = year,
            ymin = hat.lci, ymax = hat.uci),
        fill = "gray80") +
    geom_ribbon(data = filter(df_lists$data, retro.peels == 0, year < df_lists$min_year_retro),
                aes(x = year, ymin = hat.lci, ymax = hat.uci),
                fill = "gray30", alpha = 0.5) +
    geom_line(data = filter(df_lists$data, hindcast == FALSE),
              aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = df_lists$hindcast_data_2,
              aes(x = year, y = hat, group = retro.peels),
              linewidth = 1, colour = "white") +
    geom_point(data = filter(df_lists$data, retro.peels == 0, year < df_lists$min_year_retro),
               aes(x = year, y = obs), pch = 21, size = 4,
               fill = "white") +
    geom_point(data = df_lists$hindcast_data_1, show.legend = FALSE,
               aes(x = year, y = obs, fill = retro),
               pch = 21, size = 4) +
    geom_point(data = df_lists$hindcast_data_1, show.legend = FALSE,
               aes(x = year, y = hat, fill = retro),
               pch = 21, size = 2) +
    geom_text(data = df_lists$mase_data,
              aes(x = x, y = y,
                  label = paste0("MASE = ", round(MASE, 3)))) +
    labs(x = "Year", y = "Index", colour = "") +
    facet_wrap(Scenario ~ Index, ncol = length(unique(df_lists$data$Index)), drop = FALSE) +
    facet_grid(rows = vars(Scenario), cols = vars(Index)) +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_val, max_val))
    .my_theme() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
}

#' Extract hindcast diagnostics from model outputs
#'
#' Internal helper that extracts hindcast diagnostic data from model
#' outputs and combines them into a single data frame.
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
#' occurrence of a logical condition and returning selected rows around
#' that transition.
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
#' Internal helper that computes Mean Absolute Scaled Error (MASE)
#' metrics from model outputs.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing MASE values for each scenario,
#'   including plotting coordinates.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom JABBA jbmase
.process_mase <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      jbmase(fit) %>% 
        mutate(
          x = 2015, 
          y = 1.9,
          Scenario = fit[[1]]$scenario
        )
    }
  )
  result <- bind_rows(temp00) %>% filter(Index != "joint")
  return(result)
}