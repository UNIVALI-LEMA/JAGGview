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
#'   \item{mult}{A data frame containing scaling factors for plotting.}
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
    sigma02x = NULL, 
    sigma02y = NULL
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
    psi01 = NULL, psi02 = NULL, sigma01 = NULL, sigma02x = NULL, 
    sigma02y = NULL
  )
  for(i in unique(tmp13$Scenario)) {
    init <- filter(tmp13, Scenario == i)
    scen <- i
    K01 <- density(init$K, adjust = 2)$x
    K02 <- density(init$K, adjust = 2)$y
    r01 <- density(init$r, adjust = 2)$x
    r02 <- density(init$r, adjust = 2)$y
    psi01 <- density(init$psi, adjust = 2)$x
    psi02 <- density(init$psi, adjust = 2)$y
    sigma01 <- density(init$sigma2, adjust = 2)$x
    sigma02 <- density(init$sigma2, adjust = 2)$y
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
    x = 0.85, 
    y = 0.88,
    K = round((temp01$sd.K/temp01$mu.K)^2/(temp00$sd.K/temp00$mu.K)^2, 3),
    r = round((temp01$sd.r/temp01$mu.r)^2/(temp00$sd.r/temp00$mu.r)^2, 3),
    psi = round((temp01$sd.psi/temp01$mu.psi)^2/(temp00$sd.psi/temp00$mu.psi)^2,
              3))

  PPMR <- data.frame(
    Scenario = temp00$Scenario,
    x = 0.85, 
    y = 0.93,
    K = round(temp01$mu.K/temp00$mu.K, 3),
    r = round(temp01$mu.r/temp00$mu.r, 3),
    psi = round(temp01$mu.psi/temp00$mu.psi, 3))
  
  mutipliers <- data.frame(
    variable = c("K", "r", "psi"),
    limit = c(8000000, 0.3, 1.6)
  )

  list(
    prior = out02,
    posterior = out03,
    PPVR = PPVR,
    PPMR = PPMR,
    mult = mutipliers
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
#' @param palette A character vector of colors used for prior and
#'   posterior distributions.
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
#' priors_posteriors_ggplot(df, var = "K", palette = c("#4285f4" "#34a853" "#ea4335"))
#' }
#'
#' @export
#' @importFrom dplyr %>% filter select rename pull all_of
#' @importFrom ggplot2 ggplot geom_area aes geom_text facet_wrap 
#' coord_cartesian labs scale_y_continuous theme element_blank
priors_posteriors_ggplot <- function(df_lists, var, palette) {
  var1 <- paste0(var, "01")
  var2 <- paste0(var, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )
  
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )
  
  mult <- df_lists$mult %>%
    filter(variable == var) %>%
    pull(limit)

  ggplot() +
    geom_area(data = prior, aes(x = value_1, y = value_2),
              fill = palette[1], alpha = 0.5, colour = "black") +
    geom_area(data = posterior, aes(x = value_1, y = value_2),
              fill = palette[2], alpha = 0.5, colour = "black") +
    geom_text(data = df_lists$PPMR,
              aes(x = x * mult, y = y * max(posterior$value_2),
                  label = paste0("PPMR = ", K))) +
    geom_text(data = df_lists$PPVR,
              aes(x = x * mult, y = y * max(posterior$value_2),
                  label = paste0("PPVR = ", K))) +
    facet_wrap(~Scenario, ncol = 3) +
    coord_cartesian(xlim = c(0, mult)) +
    labs(x = var, y = "Density") +
    scale_y_continuous(expand = c(0, 0)) +
    .my_theme() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
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