#' Prepare prior and posterior distributions data
#'
#' Processes model outputs to generate prior and posterior distributions
#' for key parameters (e.g., K, r, psi), along with summary metrics for
#' prior-posterior comparisons.
#'
#' @param list_models A list containing model outputs as returned by the 
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
#' Prior distributions are simulated using log-normal and gamma
#' distributions based on model settings, while posterior distributions
#' are estimated using kernel density methods. Summary metrics (PPVR and
#' PPMR) are computed to assess the influence of priors on posterior
#' estimates.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_models <- list(fit.S01, fit.S02)
#' df <- priors_posteriors_data(list_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% filter summarise
#' @importFrom stats dlnorm dgamma density rlnorm sd
priors_posteriors_data <- function(list_models) {
  ###@> Filtering the expected data...
  .validate_fits_input_data(list_models)

  #####@> Priors...
  tmp12 <- .process_priors(list_models)

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
  tmp13 <- .process_posteriors(list_models)

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

  list(
    prior = out02,
    posterior = out03,
    PPVR = PPVR,
    PPMR = PPMR
  )
}

#' Plot prior and posterior distributions
#'
#' Creates a ggplot2-based visualization comparing prior and posterior
#' distributions for a selected parameter across scenarios.
#'
#' @param df_lists A named list as returned by
#'   \code{priors_posteriors_data()}.
#' @param var A character string specifying the parameter to plot.
#'   Supported values include "K", "r", and "psi".
#' @param title_x A character string for the x-axis label. If \code{NULL},
#'   a default label is assigned based on \code{variable}.
#' @param palette A character vector of colors used for prior and
#'   posterior distributions.
#' @param x_lim Optional. A numeric value (positive) used to restrict the x-axis range
#'   in the plot.
#'
#' @return A ggplot object displaying prior and posterior densities,
#'   annotated with prior-posterior metrics (PPMR and PPVR).
#'
#' @details
#' The plot overlays prior and posterior density curves, includes
#' annotations for prior-posterior mean and variance ratios, and
#' faceted views by scenario.
#'
#' @examples
#' \dontrun{
#' df <- priors_posteriors_data(list_models)
#' priors_posteriors_ggplot(df, var = "K", palette = c("#4285f4", "#34a853"))
#' }
#'
#' @export
#' @importFrom dplyr %>% filter select rename pull all_of
#' @importFrom ggplot2 ggplot geom_area aes geom_text facet_wrap 
#' coord_cartesian labs scale_y_continuous theme element_blank
priors_posteriors_ggplot <- function(
  df_lists, var, title_x = NULL, palette = c("#4285f4", "#34a853"), x_lim = NULL
) {
  if(!var %in% c("K", "r", "psi")) {
    stop("Parameter 'var' was expecting 'K', 'r' or 'psi'.")
  }

  .is_palette_valid(palette)

  if(!is.null(x_lim)) {
    if(x_lim <= 0 || !inherits(x_lim, "numeric")) {
      stop("Expected parameter 'x_lim' to be a positive number.")
    }
  }
  # if(x_lim <= 0 && !is.null(x_lim) || !inherits(x_lim, "numeric")) {
  #   stop("Expected parameter 'x_lim' to be a positive number.")
  # }

  var1 <- paste0(var, "01")
  var2 <- paste0(var, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )
  
  labels_x <- list(
    K = "Carrying capacity (K)",
    r = "Intrinsic growth rate (r)",
    psi = "Initial biomass depletion ratio (psi)"
  )

  if (is.null(title_x)) {
    title_x <- labels_x[[var]]
  }
  
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )

  pos_ppmr <- .auto_text_position(
    data_list = list(prior, posterior), 
    col_x = "value_1", 
    col_y = "value_2", 
    margin = 0.2, 
    multiplier = 1.05,
    x_max = x_lim
  )

  pos_ppvr <- .auto_text_position(
    data_list = list(prior, posterior), 
    col_x = "value_1", 
    col_y = "value_2", 
    margin = 0.2, 
    multiplier = 1, 
    x_max = x_lim
  )
  
  if(is.null(x_lim)) {
    prior_max <- max(prior$value_1, na.rm = TRUE)
    pos_max <- max(posterior$value_1, na.rm = TRUE)
    x_lim <- ifelse(prior_max > pos_max, prior_max, pos_max)
  }

  x_decimals <- ifelse(x_lim > 10, 0, 1)

  max_pos_val <- .round_to_nearest(max(posterior$value_2, na.rm = TRUE), TRUE, 1.1)
  min_pos_val <- .round_to_nearest(min(posterior$value_2, na.rm = TRUE), FALSE, 1.1)

  max_prior_val <- .round_to_nearest(max(prior$value_2, na.rm = TRUE), TRUE, 1.1)
  min_prior_val <- .round_to_nearest(min(prior$value_2, na.rm = TRUE), FALSE, 1.1)

  max_val <- if(max_pos_val > max_prior_val) {
    max_pos_val
  }
  else {
    max_prior_val
  }

  min_val <- if(min_pos_val < min_prior_val) {
    min_pos_val
  }
  else {
    min_prior_val
  }

  ggplot() +
    geom_area(data = prior, aes(x = value_1, y = value_2),
              fill = palette[1], alpha = 0.5, colour = "black") +
    geom_area(data = posterior, aes(x = value_1, y = value_2),
              fill = palette[2], alpha = 0.5, colour = "black") +
    geom_text(data = df_lists$PPMR,
              aes(x = pos_ppmr$x, y = pos_ppmr$y,
                  label = paste0("PPMR = ", K))) +
    geom_text(data = df_lists$PPVR,
              aes(x = pos_ppvr$x, y = pos_ppvr$y,
                  label = paste0("PPVR = ", K))) +
    facet_wrap(~Scenario, ncol = 3) +
    coord_cartesian(xlim = c(0, x_lim)) +
    labs(x = title_x, y = "Density") +
    scale_x_continuous(labels = function(x) .format_number(x, decimals = x_decimals)) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_val, max_val)) +
    .my_theme() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

#' Extract prior settings from model outputs
#'
#' Internal helper that extracts prior distribution parameters from
#' a list of model outputs and combines them into a single data frame.
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
#' Internal helper that extracts posterior parameter samples from a
#' list of model outputs and combines them into a single data frame.
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