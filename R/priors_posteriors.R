#' Prepare prior and posterior distributions data
#'
#' Processes model outputs to generate prior and posterior distributions for 
#' key parameters (e.g., K, r, psi), along with summary metrics for
#' prior-posterior comparisons.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{prior}{A data frame containing sampled prior distributions.}
#'   \item{posterior}{A data frame containing posterior density estimates.}
#'   \item{PPVR}{A data frame with prior-posterior variance ratios.}
#'   \item{PPMR}{A data frame with prior-posterior mean ratios.}
#' }
#'
#' @details
#' Prior distributions are simulated using log-normal and gamma distributions 
#' based on model settings, while posterior distributions are estimated using 
#' kernel density methods. Summary metrics (PPVR and PPMR) are computed to 
#' assess the influence of priors on posterior estimates.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- priors_posteriors_data(list_fit_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter summarise
#' @importFrom stats dlnorm dgamma density rlnorm sd
priors_posteriors_data <- function(list_fit_models) {
  # ###@> Filtering the expected data...
  # .validate_fits_input_data(list_fit_models)
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  #####@> Priors...
  tmp12 <- .process_priors(list_fit_models)

  out02 <- data.frame(
    Scenario = NULL, 
    K01 = NULL, 
    K02 = NULL, 
    r01 = NULL,
    r02 = NULL, 
    psi01 = NULL, 
    psi02 = NULL, 
    sigma01 = NULL,
    sigma02 = NULL
    # sigma02x = NULL, 
    # sigma02y = NULL
  )
  for(i in unique(tmp12$Scenario)) {
      init <- filter(tmp12, Scenario == i)
      scen <- i
      K01 <- sort(rlnorm(10000, log(init$K.pr[1]), init$K.pr[2]))
      K02 <- dlnorm(K01, log(init$K.pr[1]), init$K.pr[2])
      r01 <- sort(rlnorm(10000, log(init$r.pr[1]), init$r.pr[2]))
      r02 <- dlnorm(r01, log(init$r.pr[1]), init$r.pr[2])
      psi01 <- sort(rlnorm(10000, log(init$psi.pr[1]), init$psi.pr[2]))
      psi02 <- dlnorm(psi01, log(init$psi.pr[1]), init$psi.pr[2])
      sigma01 <- seq(0.0001, 1, l = 10000)
      sigma02 <- dgamma(sigma01, init$proc.pr[1], init$proc.pr[2], log = TRUE)
      out02 <- rbind(
        out02, 
        data.frame(
          Scenario = scen,
          K01 = K01,
          K02 = K02,
          r01 = r01,
          r02 = r02,
          psi01 = psi01,
          psi02 = psi02,
          sigma01 = sigma01,
          sigma02 = sigma02
        )
      )
  }

  #####@> Posteriors...
  tmp13 <- .process_posteriors(list_fit_models)

  out03 <- data.frame(
    Scenario = NULL, K01 = NULL, K02 = NULL, r01 = NULL,r02 = NULL, 
    psi01 = NULL, psi02 = NULL, sigma01 = NULL, sigma02 = NULL # sigma02x = NULL, 
    # sigma02y = NULL
  )
  for(i in unique(tmp13$Scenario)) {
    init <- filter(tmp13, Scenario == i)
    scen <- i
    K_density <- density(init$K, adjust = 2)
    r_density <- density(init$r, adjust = 2)
    psi_density <- density(init$psi, adjust = 2)
    sigma_density <- density(init$sigma, adjust = 2)
    K01 <- K_density$x
    K02 <- K_density$y
    r01 <- r_density$x
    r02 <- r_density$y
    psi01 <- psi_density$x
    psi02 <- psi_density$y
    sigma01 <- sigma_density$x
    sigma02 <- sigma_density$y
    out03 <- rbind(
      out03, data.frame(
        Scenario = scen, 
        K01 = K01, 
        K02 = K02, 
        r01 = r01, 
        r02 = r02, 
        psi01 = psi01, 
        psi02 = psi02, 
        sigma01 = sigma01, 
        sigma02 = sigma02
      )
    )
  }

  #####@> PPVR and PPVM...
  temp00 <- out02 %>%
    summarise(
      mu.K = mean(K01),
      sd.K = sd(K01),
      mu.r = mean(r01),
      sd.r = sd(r01),
      mu.psi = mean(psi01),
      sd.psi = sd(psi01),
      .by = Scenario
    )
  temp01 <- tmp13 %>%
    summarise(
      mu.K = mean(K),
      sd.K = sd(K),
      mu.r = mean(r),
      sd.r = sd(r),
      mu.psi = mean(psi),
      sd.psi = sd(psi),
      .by = Scenario
    )
  
  PPVR <- data.frame(
    Scenario = temp00$Scenario, 
    K = round((temp01$sd.K/temp01$mu.K)^2/(temp00$sd.K/temp00$mu.K)^2, 3),
    r = round((temp01$sd.r/temp01$mu.r)^2/(temp00$sd.r/temp00$mu.r)^2, 3),
    psi = round((temp01$sd.psi/temp01$mu.psi)^2/(temp00$sd.psi/temp00$mu.psi)^2,
              3))

  PPMR <- data.frame(
    Scenario = temp00$Scenario,
    K = round(temp01$mu.K/temp00$mu.K, 3),
    r = round(temp01$mu.r/temp00$mu.r, 3),
    psi = round(temp01$mu.psi/temp00$mu.psi, 3))

  results <- list(
    prior = out02,
    posterior = out03,
    PPVR = PPVR,
    PPMR = PPMR
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Plot prior and posterior distributions
#'
#' Creates a ggplot2-based visualization comparing prior and posterior
#' distributions for a selected parameter across scenarios.
#'
#' @param df_lists A named list as returned by \code{priors_posteriors_data()}.
#' @param indicator_name A character string specifying the parameter to plot.
#'   Supported values include "K", "r", and "psi".
#' @param use_si_suffix A boolean value indicating whether SI suffixes will be 
#'   used, or if FALSE then shows the absolute number, Defaults to FALSE.
#' @param text_size An integer value that determines the size of the text. 
#'   Defaults to 4.
#' @param palette Optional. A character vector of colors used for plotting. 
#'   If \code{NULL} (default), a color-blind-friendly palette is generated
#'   automatically according to the number of index levels.
#'   If the number of suplied colors is smaller than the number specified, 
#'   than the code returns an error.
#' @param title_x A character string for the x-axis label. If \code{NULL}, a 
#'   default label is assigned based on \code{indicator_name}.
#' @param x_decimals Optional. Number of decimal places.
#' @param x_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the x-axis c(min, max) used to restrict the plotting range.
#' @param y_lim Optional. A numeric vector of length 2 specifying the lower and 
#'   upper limits of the y-axis c(min, max) used to restrict the plotting range.
#'
#' @return A ggplot object displaying prior and posterior densities, annotated 
#'   with prior-posterior metrics (PPMR and PPVR).
#'
#' @details
#' The plot overlays prior and posterior density curves, includes annotations 
#' for prior-posterior mean and variance ratios, and faceted views by scenario.
#'
#' @examples
#' \dontrun{
#' df <- priors_posteriors_data(list_fit_models)
#' priors_posteriors_ggplot(df, "K", TRUE, palette = c("#4285f4", "#34a853"))
#' }
#'
#' @export
#' @importFrom dplyr %>% filter select rename pull all_of
#' @importFrom ggplot2 ggplot geom_area aes facet_wrap geom_text
#' coord_cartesian labs scale_y_continuous theme element_blank
priors_posteriors_ggplot <- function(
  df_lists, indicator_name, use_si_suffix = FALSE, text_size = 4,
  palette = NULL, title_x = NULL, x_decimals = NULL, x_lim = NULL, y_lim = NULL
) {
  if (!inherits(df_lists, "JAGGdata")) {
    stop("Input data was expected to have 'JAGGdata' class.")
  }
  if (!indicator_name %in% c("K", "r", "psi")) {
    stop("Parameter 'indicator_name' was expecting 'K', 'r' or 'psi'.")
  }

  palette <- .resolve_palette(palette, 2)

  .axis_limit(x_lim)

  .axis_limit(y_lim)

  indicator1 <- paste0(indicator_name, "01")
  indicator2 <- paste0(indicator_name, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(
      value_1 = all_of(indicator1),
      value_2 = all_of(indicator2)
    )
  
  labels_x <- list(
    K = "Carrying capacity (K)",
    r = "Intrinsic growth rate (r)",
    psi = "Initial biomass depletion ratio (psi)"
  )

  if (is.null(title_x)) {
    title_x <- labels_x[[indicator_name]]
  }
  
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(indicator1, indicator2)))) %>%
    rename(
      value_1 = all_of(indicator1),
      value_2 = all_of(indicator2)
    )
  
  if (is.null(x_lim)) {
    prior_x_max <- max(prior$value_1, na.rm = TRUE)
    pos_x_max <- max(posterior$value_1, na.rm = TRUE)
    x_lim <- c(0, ifelse(prior_x_max > pos_x_max, prior_x_max, pos_x_max))
  }

  if (is.null(x_decimals)) {
    x_decimals <- ifelse(x_lim[2] > 10, 0, 2)
  }

  if (is.null(y_lim)) {
    max_y_pos <- .round_to_nearest(max(posterior$value_2, na.rm = TRUE), 
                                    TRUE, 1.1)
    min_y_pos <- .round_to_nearest(min(posterior$value_2, na.rm = TRUE), 
                                    FALSE, 1.1)

    max_prior <- .round_to_nearest(max(prior$value_2, na.rm = TRUE), 
                                      TRUE, 1.1)
    min_prior <- .round_to_nearest(min(prior$value_2, na.rm = TRUE), 
                                      FALSE, 1.1)

    max_y_val <- if (max_y_pos > max_prior) {
      max_y_pos
    }
    else {
      max_prior
    }

    min_y_val <- if (min_y_pos < min_prior) {
      min_y_pos
    }
    else {
      min_prior
    }
    y_lim <- c(min_y_val, max_y_val)
  }

  pos <- .auto_text_position(
    data_list = list(prior, posterior), 
    col_x = "value_1", 
    col_y = "value_2",
    xlim = x_lim,
    ylim = y_lim, 
    margin = 0.2
  )
  
  df_text <- df_lists$PPMR %>%
  select(Scenario, ppmr_value = all_of(indicator_name)) %>%
  full_join(
    df_lists$PPVR %>%
      select(Scenario, ppvr_value = all_of(indicator_name)),
    by = "Scenario"
  ) %>%
  mutate(
    x = pos$x,
    y = pos$y
  )

  x_labels <- if (use_si_suffix) {
    function(x) .international_system_prefixes(x)
  } else {
    function(x) .format_number(x, decimals = x_decimals)
  }
  
  ggplot() +
    geom_area(data = prior, aes(x = value_1, y = value_2),
              fill = palette[1], alpha = 0.5, colour = "black") +
    geom_area(data = posterior, aes(x = value_1, y = value_2),
              fill = palette[2], alpha = 0.5, colour = "black") +
    geom_text(data = df_text,
                    aes(x = x, y = y, 
                        label = paste0("PPMR = ", ppmr_value)), size = text_size
    ) +
    geom_text(data = df_text,
                    aes(x = x, y = y, 
                        label = paste0("PPVR = ", ppvr_value)), vjust = 2,
                        size = text_size
    ) +
    facet_wrap(~Scenario, ncol = 3) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    labs(x = title_x, y = "Density") +
    scale_x_continuous(labels = x_labels) +
    scale_y_continuous(expand = c(0, 0)) +
    .my_theme() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
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
#' @export
get_ppvr <- function(df_lists) {
  return(df_lists$PPVR)
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
#' @export
get_ppmr <- function(df_lists) {
  return(df_lists$PPMR)
}

#' Extract prior settings from model outputs
#'
#' Internal helper that extracts prior distribution parameters from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing prior parameters for each scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_priors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        K.pr = fit$settings$K.pr,
        r.pr = fit$settings$r.pr,
        psi.pr = fit$settings$psi.pr,
        psi.dist = fit$settings$psi.dist,
        proc.pr = fit$settings$igamma
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}

#' Extract posterior samples from model outputs
#'
#' Internal helper that extracts posterior parameter samples from a list of 
#' model outputs and combines them into a single data frame.
#'
#' @param fit_list A list of model outputs.
#'
#' @return A data frame containing posterior samples for each scenario.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.process_posteriors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        fit$pars_posterior
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}