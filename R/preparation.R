#' Prepare fitted index data and credibility intervals
#' 
#' Extracts observed indices, fitted values and uncertainty from one or more 
#' JABBA fits and returns a list with data frames ready for 
#' \code{\link{fits_ggplot}()}.
#'
#' @details
#' Credibility intervals in \code{Li_Ui} assume normality: the standard error
#' is the square root of the \code{SE2} variance, multiplied by 1.96.
#' \code{CI_80} and \code{CI_95} are taken from the posterior predictive
#' (\code{ppd}) and fitted (\code{hat}) values, respectively.
#'
#' Missing standard errors are filled using the observed index series.
#' Incomplete rows are dropped from \code{Li_Ui}.
#'
#' If \code{indices_factor} is supplied, the \code{Index} column is reordered
#' following that vector.
#'
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario).
#' @param indices_factor Optional. A character vector with the index names to 
#'   use and their order. All values must exist in the \code{Index} column.
#' 
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{Li_Ui}{Mean values and lower (\code{Li}) and upper (\code{Ui}) 
#'   bounds.}
#'   \item{CI_80}{Fitted values with 80% credibility intervals.}
#'   \item{CI_95}{Fitted values with 95% credibility intervals.}
#' }
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
#' res1 <- fits_data(fit.S01)
#' 
#' # A list of fits (one per scenario)
#' res2 <- fits_data(list(fit.S01, fit.S02))
#' }
#' 
#' @family preparation functions
#' @family fits functions
#' 
#' @export
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr %>% full_join mutate select 
#' @importFrom stats complete.cases
#' @importFrom forcats fct_relevel
fits_data <- function(list_fit_models, indices_factor = NULL) {
  # Accepts a single fit by wrapping it into a list
  if (.is_fit_jabba(list_fit_models)) list_fit_models <- list(list_fit_models)

  # Observed indices (mean) and standart errors (variance) per scenario
  tmp01 <- .process_scenarios(list_fit_models, vars = "I")
  tmp02 <- .process_scenarios(list_fit_models, vars = "SE2")

  # Keep NA paterns in SE consistent with the index data
  tmp02 <- .replace_na_with_na(tmp02, tmp01)

  # Reshape to long format
  tmp01 <- pivot_longer(
    data = tmp01, names_to = "Index", values_to = "Mean", 3:ncol(tmp01)
  )
  tmp02 <- pivot_longer(
    data = tmp02, names_to = "Index", values_to = "SE", 3:ncol(tmp02)
  )
  tmp00 <- full_join(tmp01, tmp02, by = c("Year", "Scenario", "Index"))

  # Verify if requested indices exist in the data 
  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp00$Index), indices_factor)
  }

  # Estimating Upper and Lower errors
  tmp00$error <- with(tmp00, (1.96 * sqrt(SE)))
  tmp00$Ui <- with(tmp00, Mean + error)
  tmp00$Li <- with(tmp00, Mean - error)

  # Extract index names from model inputs
  index_inputseries <- .process_index(list_fit_models)

  if (any(is.na(tmp00$Index))) .fill_na_indices(tmp00, index_inputseries)

  tmp00 <- tmp00[complete.cases(tmp00),] %>% 
    mutate(
      Index = fct_relevel(Index, indices_factor),
      Year = as.integer(Year)
    ) %>%
    select(-SE)

  # Extract and combine CPUE data for the 80% interval
  tmp03 <- .process_cpues(list_fit_models, vars = "ppd")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp03$Index), indices_factor)
  }

  if (any(is.na(tmp03$Index))) .fill_na_indices(tmp03, index_inputseries)

  tmp03 <- tmp03 %>% 
    mutate(
      Index = fct_relevel(Index, indices_factor),
      Year = as.integer(Year)
    ) %>% 
    select(-c(se, obserror))

  # Extract and combine CPUE data for the 95% interval
  tmp04 <- .process_cpues(list_fit_models, vars = "hat")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp04$Index), indices_factor)
  }

  if (any(is.na(tmp04$Index))) {
    tmp04 <- .fill_na_indices(tmp04, index_inputseries)
  }

  tmp04 <- tmp04 %>% 
    mutate(
      Index = fct_relevel(Index, indices_factor),
      Year = as.integer(Year)
    )

  results <- list(
    Li_Ui = tmp00,
    CI_80 = tmp03,
    CI_95 = tmp04
  )
  class(results) <- c("JAGGdata", class(results))

  # Fail if every data frame is entirely NA
  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }
  
  return(results)
}

#' Prepare hindcast analysis data
#' 
#' Extracts observed and predicted values from one or more JABBA hindcast runs 
#' and computes the Mean Absolute Scaled Error (MASE) for each index. The 
#' result is ready for \code{\link{hindcast_ggplot}()}.  
#'
#' @details
#' Hindcast runs are extracted and the retrospective labels are formatted. MASE 
#' is computed with \code{JABBA::jbmase()}. Indices whose MASE is \code{NA} are 
#' removed from all returned data frames.
#' 
#' If \code{indices_factor} is supplied, the \code{Index} column is reordered
#' following that vector.
#' 
#' @param list_hc_models Either a single fit returned by 
#'   \code{JABBA::hindcast_jabba()}, or a list of such hindcasts (one per 
#'   scenario). A single hindcast is automatically wrapped into a list.
#' @param indices_factor Optional. A character vector with the index names to 
#'   use and their order. All values must exist in the \code{Index} column.
#'
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{data}{Full hindcast time series, with observed and predicted values.}
#'   \item{data_points}{Selected hindcast points (first hindcast year of each
#'   retrospective run) used to plot observed and predicted values.}
#'   \item{data_lines}{Filtered hindcast trajectories used for plotting lines.}
#'   \item{mase_data}{Mean Absolute Scaled Error (MASE) metrics for each index 
#'   and scenario.}
#' }
#'
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- hindcast_jabba(jb.S01, fit.S01, ...)
#' 
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- hindcast_jabba(jb.S02, fit.S01, ...)
#' 
#' # A single hindcast
#' res1 <- hindcast_data(hc_S01)
#' 
#' # A list of hindcasts (one per scenario)
#' res2 <- hindcast_data(list(hc_S01, hc_S02))
#' }
#' 
#' @family preparation functions
#' @family hindcasts functions
#'
#' @export
#' @importFrom dplyr %>% filter group_by mutate pull rename ungroup
#' @importFrom forcats fct_relevel
hindcast_data <- function(list_hc_models, indices_factor = NULL) {
  # Accepts a single fit by wrapping it into a list
  if (.is_hindcast_jabba(list_hc_models)) list_hc_models <- list(list_hc_models)

  # Observed and predicted values per retrospective run (peel)
  hc <- .process_hindcasts(list_hc_models)

  # Reference year derived from the peel labels
  min_year <- as.integer(gsub("-", "", min(hc$Peel))) - 1  

  # MASE per index and scenario
  mase <- .process_mase(list_hc_models)

  # Indices with undefined MASE are dropped from the results
  na_index <- mase %>%
    filter(is.na(MASE)) %>%
    pull(unique(Index))

  # Standardize column names
  tmp00 <- hc %>%
    rename(
      retro = Peel,
      Scenario = level,
      Index = name
    )
  
  # Verify if requested indices exist in the data 
  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp00$Index), indices_factor)
  }

  # Remove indices without MASE and set factor orders
  tmp00 <- tmp00 %>% 
    filter(!Index %in% na_index) %>%
    mutate(
      retro = fct_relevel(retro, sort(unique(retro), decreasing = TRUE)),
      Index = fct_relevel(Index, indices_factor)
    )
  
  # Hindcast points: first hindcast year of each retrospective run
  tmp01 <- tmp00 %>%
    filter(hindcast == TRUE, year > min_year) %>%
    group_by(retro.peels) %>%
    filter(year == min(year)) %>%
    ungroup()
  
  # Hindcast lines: trajectories filtered by peel, hindcast flag and year
  tmp02 <- .filter_by_condition(tmp00, "retro.peels", "hindcast", "year")

  results <- list(
    data = tmp00,
    data_points = tmp01,
    data_lines = tmp02,
    mase_data = mase  %>% 
      filter(!Index %in% na_index),
    min_year_retro = min_year
  )

  class(results) <- c("JAGGdata", class(results))

  # Fail if every element is entirely NA
  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }
  
  return(results)
}

#' Prepare data for Kobe plot visualization
#'
#' Computes median B/Bmsy and F/Fmsy ratios by year and scenario, and builds 
#' the components of a Kobe plot: reference quadrants and credibility contours
#' for the terminal year. The result is ready for \code{\link{kobe_ggplot}()}.
#' 
#' @details
#' Contours are estimated with kernel density (\code{gplots::ci2d()}) using 
#' the posterior samples of the terminal year, one set per scenario.
#' 
#' Memory use is monitored while the posterior samples are extracted. If the
#' free memory falls below \code{reserve_mb}, the process is aborted with a
#' message and \code{NULL} is returned.
#'
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario).
#' @param ci_levels A numeric vector with the credibility levels of the 
#'   contours, strictly between 0 and 1. Defaults to \code{c(0.5, 0.8, 0.95)}.
#' @param reserve_mb A numeric value for the minimum free system memory, in 
#'   megabytes, to keep available. Defaults to 2048.
#' @param poll_interval A numeric value giving the time interval, in seconds, 
#'   between memory availability checks. Defaults to 0.5.
#'
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{col01}{Yellow Kobe quadrant (B/Bmsy < 1 and F/Fmsy < 1: overfished, 
#'   not overfishing).}
#'   \item{col02}{Orange quadrant (B/Bmsy > 1 an F/Fmsy > 1: not overfished and 
#'   overfishing).}
#'   \item{col03}{Red quadrant (B/Bmsy < 1 and F/Fmsy > 1: overfished and 
#'   overfishing).}
#'   \item{col04}{Green quadrant (B/Bmsy > 1 and F/Fmsy < 1, not overfished and 
#'   not overfishing).}
#'   \item{ci_data}{Contour coordinates for the terminal year, by scenario and 
#'   credibility level (\code{q}).}
#'   \item{data_lines}{Median B/Bmsy (\code{Bratio}) and F/Fmsy 
#'   (\code{Fratio}) by year and scenario.}
#'   \item{highlight_years}{Subset of \code{data_lines} for the first, middle 
#'   and last year of the series.}
#' }
#' Returns \code{NULL} if the process is aborted due to low memory.
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
#' res1 <- kobe_data(fit.S01)
#' 
#' # A list of fits (one per scenario), with custom credibility levels
#' res2 <- kobe_data(list(fit.S01, fit.S02), ci_levels = c(0.5, 0.95))
#' }
#' 
#' @family preparation functions
#' @family kobe functions
#'
#' @export
#' @importFrom dplyr %>% summarise arrange filter rename mutate
#' @importFrom stats median
#' @importFrom gplots ci2d
#' @importFrom forcats fct_relevel
kobe_data <- function(
  list_fit_models, ci_levels = c(0.5, 0.8, 0.95), reserve_mb = 2048, 
  poll_interval = 0.5
) {
  # Validate arguments before the extraction
  if (!inherits(ci_levels, "numeric")) {
    stop("Parameter 'ci_levels' was expecting a numeric vector")
  }
  if (any(is.na(ci_levels))) {
    stop("Parameter 'ci_levels' cannot contain NA.")
  }
  if (any(ci_levels <= 0 | ci_levels >= 1)) {
    stop("Parameter 'ci_levels' was expecting numbers between 0 and 1.")
  }

  # Posterior samples of B/Bmsy and F/Fmsy, aborting on low memory
  model_results <- tryCatch({
    .safe_execute(
      fn = function(kb) {
        .jbplot_ensemble2(
          kb = kb,
          kbout = TRUE,
          plot = FALSE
        )
      },
      args = list(
        kb = list_fit_models
      ),
      reserve_mb = reserve_mb,
      poll_interval = poll_interval
    )
  }, memoryLimitExceeded = function(e) {
    message("Process aborted to prevent the system from running out of memory")
    NULL
  })
  if (is.null(model_results)) return(invisible(NULL))

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

  # Median ratios by year and scenario
  data_lines <- model_results %>%
    summarise(
      Fratio = median(harvest),
      Bratio = median(stock),
      .by = c(year, Scenario)
    ) %>%
    arrange(Scenario, year)

  # Extract limits for the quadrants
  max_x <- ceiling(max(data_lines$Bratio))
  max_y <- ceiling(max(data_lines$Fratio))
  
  # Kobe quadrant (one row each)
  col01 <- data.frame(
    xmin = 0, xmax = 1, ymin = 0, ymax = 1, col = "yellow"
  )
  col02 <- data.frame(
    xmin = 1, xmax = max_x, ymin = 1, ymax = max_y, col = "orange"
  )
  col03 <- data.frame(
    xmin = 0, xmax = 1, ymin = 1, ymax = max_y, col = "red"
  )
  col04 <- data.frame(
    xmin = 1, xmax = max_x, ymin = 0, ymax = 1, col = "#00FF00"
  )

  # First, middle and last year to highlight on the plot
  max_year <- max(model_results$year)
  min_year <- min(model_results$year)
  mid_year <- round(min_year + (max_year - min_year)/2)
  highlight_years <- filter(
    data_lines, year %in% c(min_year, mid_year, max_year)
  )

  # Kernel density contours of the terminal year, by scenario
  ci_data <- data.frame(x = NULL, y = NULL, Scenario = NULL, q = NULL)
  for(i in unique(model_results$Scenario)) {
    x <- filter(model_results, Scenario == i)
    x <- filter(x, year == max_year)
    kernelF <- ci2d(
      x$stock, 
      x$harvest, 
      nbins = 151,
      factor = 1.5,
      ci.levels = ci_levels,
      show = "none",
      col = 1
    )

    tmp00 <- lapply(
      ci_levels, function(ci) {
        q <- kernelF$contours[[as.character(ci)]]
        q$Scenario <- i
        q$q <- paste0(ci*100, "%")
        q
      })
    
    tmp <- do.call(rbind, tmp00)

    ci_data <- rbind(
      ci_data, 
      data.frame(
        x = tmp$x,
        y = tmp$y,
        Scenario = tmp$Scenario,
        q = fct_relevel(tmp$q, sort(unique(tmp$q), decreasing = TRUE))
      )
    )
  }
  
  results <- list(
    col01 = col01,
    col02 = col02,
    col03 = col03,
    col04 = col04,
    ci_data = ci_data,
    data_lines = data_lines,
    highlight_years = highlight_years
  )

  class(results) <- c("JAGGdata", class(results))

  # Fail if every element is entirely NA
  if (all(is.na(results))) {
    stop("Data frame only have NA data.")
  }
  return(results)
}

#' Prepare prior and posterior distributions data
#'
#' Simulates the prior distributions and estimates the posterior densities of 
#' the main JABBA parameters (K, R, psi and sigma) for each scenario, and 
#' computes summary ratios comparing them. The result is ready for 
#' \code{\link{priors_posteriors_ggplot}()}.
#' 
#' @details
#' Priors for K, r and psi are simulated from log-normal distributions (10,000
#' draws) and the prior for sigma is evaluated on a gamma density, using the 
#' settings stored in each fit. Posterior densities are estimated with 
#' \code{stats::density()} (\code{adjust = 2}).
#' 
#' In \code{prior} and \code{posterior}, columns ending in \code{01} hold the 
#' parameter values amd columns ending in \code{02} hold the density. For sigma 
#' in \code{prior}, \code{sigma02} is the log-density.
#' 
#' Since priors are simulated randomly, \code{PPVR} and \code{PPMR} can vary
#' slightly between runs. Use \code{set.seed()} for reproducible results.
#'
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario).
#'
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{prior}{Simulated prior values and densities, by scenario.}
#'   \item{posterior}{Posterior density estimates, by scenario.}
#'   \item{PPVR}{Prior-posterior variance ratio (posterior squared coefficient
#'   of variation divided by the prior one) for K, r and psi.}
#'   \item{PPMR}{Prior-posterior mean ratio (posterior mean divided by prior 
#'   mean) for K, r and psi.}
#' }
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
#' res1 <- priors_posteriors_data(fit.S01)
#' 
#' # A list of fits (one per scenario)
#' res2 <- priors_posteriors_data(list(fit.S01, fit.S02))
#' }
#' 
#' @family preparation functions
#' @family priors vs posteriors functions
#'
#' @export
#' @importFrom dplyr %>% filter summarise
#' @importFrom stats dlnorm dgamma density rlnorm sd
priors_posteriors_data <- function(list_fit_models) {
  # Accepts a single fit by wrapping it into a list
  if (.is_fit_jabba(list_fit_models)) list_fit_models <- list(list_fit_models)

  # Priors: simulated values (01) and densities (02), by scenario
  tmp12 <- .process_priors(list_fit_models)

  priors <- data.frame(
    Scenario = NULL, 
    K01 = NULL, 
    K02 = NULL, 
    r01 = NULL,
    r02 = NULL, 
    psi01 = NULL, 
    psi02 = NULL, 
    sigma01 = NULL,
    sigma02 = NULL
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
      priors <- rbind(
        priors, 
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

  # Posteriors: kernel density of the MCMC samples, by scenario
  tmp13 <- .process_posteriors(list_fit_models)

  posteriors <- data.frame(
    Scenario = NULL, K01 = NULL, K02 = NULL, r01 = NULL,r02 = NULL, 
    psi01 = NULL, psi02 = NULL, sigma01 = NULL, sigma02 = NULL
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
    posteriors <- rbind(
      posteriors, data.frame(
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

  # Mean and standart deviation of priors and posteriors
  temp00 <- priors %>%
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
  
  # Prior-posterior variance ratio (ratio of squared CVs)
  PPVR <- data.frame(
    Scenario = temp00$Scenario, 
    K = round((temp01$sd.K/temp01$mu.K)^2/(temp00$sd.K/temp00$mu.K)^2, 3),
    r = round((temp01$sd.r/temp01$mu.r)^2/(temp00$sd.r/temp00$mu.r)^2, 3),
    psi = round(
      (temp01$sd.psi/temp01$mu.psi)^2/(temp00$sd.psi/temp00$mu.psi)^2,3
    )
  )

  # Prior-posterior mean ratio
  PPMR <- data.frame(
    Scenario = temp00$Scenario,
    K = round(temp01$mu.K/temp00$mu.K, 3),
    r = round(temp01$mu.r/temp00$mu.r, 3),
    psi = round(temp01$mu.psi/temp00$mu.psi, 3)
  )

  results <- list(
    prior = priors,
    posterior = posteriors,
    PPVR = PPVR,
    PPMR = PPMR
  )

  class(results) <- c("JAGGdata", class(results))

  # Fail if every data frame is entirely NA
  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Prepare retrospective analysis data
#'
#' Extracts time series from one or more JABBA hindcast runs, together with 
#' surplus production curves and retrospective bias metrics (Mohn's rho), for
#' each scenario. The result is ready for 
#' \code{\link{retrospective_analysis_ggplot}()}.
#' 
#' @details
#' Only the quantities B, F, B/Bmsy, F/Fmsy and the process error on 
#' log(biomass) are kept in \code{data}. Each retrospective run (peel) is 
#' identified by the \code{id} column (\code{"Ref"} for the reference run, 
#' \code{"-1"}, \code{"-2"}, ... for the peels).
#'
#' The logical column \code{keep} flags the years to be drawn in the 
#' retrospective lines: it is \code{TRUE} for all years of the reference run 
#' and, for each peel, only years earlier than the peel's terminal year
#' (\code{Year < id_num}). 
#'
#' @param list_hc_models Either a single hindcast returned by
#'   \code{JABBA::hindcast_jabba()}, or a list of such hindcasts (one per
#'   scenario). A single hindcast is automatically wrapped into a list.
#'
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{data}{Time series B, F, B/Bmsy, F/Fmsy, and process error, by 
#'   scenario and retrospective run, with the labels in \code{Index2} and the 
#'   logical filter \code{keep}.}
#'   \item{surplus_data}{Surplus production (MSY-related) curves, by scenario 
#'   and retrospective run.}
#'   \item{rho_data}{retrospective bias estimates (rho) by index and scenario.}
#' }
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
#' res1 <- retrospective_analysis_data(hc_S01)
#'
#' # A list of hindcasts (one per scenario)
#' res2 <- retrospective_analysis_data(list(hc_S01, hc_S02))
#' }
#' 
#' @family preparation functions
#' @family retrospective analysis functions
#'
#' @export
#' @importFrom dplyr %>% filter mutate select
#' @importFrom forcats fct_relevel
retrospective_analysis_data <- function(list_hc_models) {
  # Accepts a single fit by wrapping it into a list
  if (.is_hindcast_jabba(list_hc_models)) list_hc_models <- list(list_hc_models)

  # Display labels for each quantity
  labels_index <- c(
    "B"      = "Biomass",
    "F"      = "Fishing Mortality",
    "BBmsy"  = "B/Bmsy",
    "FFmsy"  = "F/Fmsy",
    "procB"  = "Process Error on log(Biomass)"
  )

  # Time series per retrospective run
  tmp17 <- .process_retro(list_hc_models) %>%
    filter(Index %in% c("B", "F", "BBmsy", "FFmsy", "procB")) %>%
    mutate(
      Index2 = labels_index[Index]
    ) %>%
    mutate(
      id = fct_relevel(id, sort(unique(id), decreasing = TRUE)),
      Year = as.integer(Year)
    ) %>%
    mutate(
      # Peel number: NA for "Ref" (avoids the coercion warning)
      id_num = {
        out <- rep(NA_integer_, length(id))
        idx <- grepl("^-\\d+$", id)
        out[idx] <- as.integer(sub("-", "", id[idx]))
        out
      },
      keep = ifelse(
        id == "Ref",
        TRUE,
        Year < id_num
      )
    ) %>%
    select(-id_num)

  # Surplus production curves per retrospective run
  tmp18 <- .process_pfunc(list_hc_models) %>%
    mutate(
      Index = "MSY",
      Index2 = "Surplus Production"
    ) %>%
    mutate(
      id = fct_relevel(id, sort(unique(id), decreasing = TRUE))
    )

  # Retrospective bias (rho) 
  temp02 <- .rho_retro(list_hc_models)

  results <- list(
    data = tmp17,
    surplus_data = tmp18,
    rho_data = temp02
  )

  class(results) <- c("JAGGdata", class(results))

  # Fail if every data frame is entirely NA
  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Prepare runs test diagnostics data
#'
#' Computes runs tests on the residuals of the CPUE indices of one or more 
#' JABBA fits, and smooths the residuals with LOESS. The result is ready for 
#' \code{\link{runs_tests_ggplot}()} and \code{\link{cpue_residuals_ggplot}()}.
#' 
#' @details
#' The runs test is computed with \code{JABBA::jbruns_sig3()} for each index 
#' and scenario.\code{lcl} and \code{ucl} are it's 3-sigma limits, and 
#' \code{pvalue} is the runs test p-value. In \code{SE3}, \code{class} is
#' \code{"red"} when \code{pvalue < 0.05} (residuals are not random) and 
#' \code{"green"} otherwise. In \code{cpue_residuals}, \code{class} is
#' \code{"red"} when the residual falls outisde the 3-sigma limits and 
#' \code{"white"} otherwise.
#' 
#' A single LOESS curve is fitted to the residuals of all indices and scenarios 
#' together (\code{Res - Year}). The bands (\code{lower} and \code{upper}) are 
#' 95% confidence bands, computed as the fit plus or minus 1.96 standart 
#' errors.
#' 
#' Rows with missing values are removed. If \code{indices_factor} is supplied, 
#' the \code{Index} column is reordered following that vector.
#'
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario).
#' @param indices_factor Optional. A character vector with the index names to 
#'   use and their order. All values must exist in the \code{Index} column.
#'
#' @return An object of class \code{JAGGdata} (a named list) containing:
#' \describe{
#'   \item{cpue_residuals}{Residuals (\code{Res}) by year, scenario and index,
#'   with the runs test limits (\code{lci}, \code{ucl}), the \code{class} of 
#'   each residual, and the LOESS fit (\code{fit}) with it's band 
#'   (\code{lower},\code{upper}).}
#'   \item{SE3}{Runs test results by index and scenario: year range 
#'   (\code{ymin}, \code{ymax}), 3-sigma limits (\code{lcl}, \code{ucl}), 
#'   \code{pvalue} and \code{class}.}
#'   \item{RMSE_data}{Root mean square error (RMSE) by index and scenario.}
#' }
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
#' res1 <- runs_tests_data(fit.S01)
#'
#' # A list of fits (one per scenario)
#' res2 <- runs_tests_data(list(fit.S01, fit.S02))
#' }
#' 
#' @family preparation functions
#' @family cpue residuals runs tests functions
#'
#' @export
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom dplyr %>% mutate filter left_join select
#' @importFrom JABBA jbruns_sig3
#' @importFrom stats loess predict
#' @importFrom forcats fct_relevel
runs_tests_data <- function(list_fit_models, indices_factor = NULL) {
  # Accepts a single fit by wrapping it into a list
  if (.is_fit_jabba(list_fit_models)) list_fit_models <- list(list_fit_models)

  # Residuals by year and scenario
  tmp05 <- .process_runs(list_fit_models)
  indices <- 4:ncol(tmp05)
  min_year <- min(tmp05$Year, na.rm = TRUE)
  max_year <- max(tmp05$Year, na.rm = TRUE)

  # Runs test (3-sigma limits and p-value) by index and scenario
  out.test <- data.frame(
    expand.grid(
      Index = names(tmp05)[indices], 
      Scenario = unique(tmp05$Scenario),
      ymin = as.integer(min_year), 
      ymax = as.integer(max_year), 
      lcl = NA, 
      ucl = NA, 
      pvalue = NA
    )
  )

  for(i in indices) {
    for(j in unique(tmp05$Scenario)) {
      name <- names(tmp05)[i]
      index <- tmp05 %>% 
        filter(Scenario == j) %>%
        select(all_of(i)) %>%
        drop_na() %>%
        pull(1)
      test <- jbruns_sig3(index, type = "resid")
      rows <- out.test$Index == name & out.test$Scenario == j
      out.test$lcl[rows] <- test$sig3lim[1]
      out.test$ucl[rows] <- test$sig3lim[2]
      out.test$pvalue[rows] <- test$p.runs
    }
  }

  out.test$class <- ifelse(out.test$pvalue < 0.05, "red", "green")
  out.test <- out.test %>% 
    drop_na()

  # Reshape residuals to long format
  tmp05 <- pivot_longer(
    tmp05, names_to = "Index", values_to = "Res", indices
  )

  # Check that requested indices exist in the data
  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp05$Index), indices_factor)
  }
  
  # Add limits, flag residuals outside them and set index order
  tmp05 <- tmp05 %>%
    filter(complete.cases(.)) %>%
    left_join(out.test, by = c("Scenario", "Index")) %>%
    select(Year:Res, lcl, ucl) %>%
    mutate(
      class = ifelse(Res < lcl | Res > ucl, "red", "white"),
      Index = fct_relevel(Index, indices_factor),
      Year = as.integer(Year)
    ) %>%
    droplevels()
  
  # LOESS smoothing of the residuals with 95% confidence bands
  loess_fit <- loess(Res ~ Year, data = tmp05)
  pred <- predict(loess_fit, se = TRUE)
  tmp05$fit   <- pred$fit
  tmp05$upper <- pred$fit + 1.96 * pred$se.fit
  tmp05$lower <- pred$fit - 1.96 * pred$se.fit

  # RMSE by index and scenario
  RMSE_data <- .process_stats(list_fit_models) %>% 
    filter(Stastistic == "RMSE")

  results <- list(
    cpue_residuals = tmp05,
    SE3 = out.test,
    RMSE_data = RMSE_data
  )

  class(results) <- c("JAGGdata", class(results))

  # Fail if every data frame is entirely NA
  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Summarise trajectory data from model outputs
#'
#' Computes the median and quantile intervals of biomass, fishing mortality and 
#' related indicators from the posterior samples of one or more JABBA fits, by 
#' year and scenario. The result is ready for 
#' \code{\link{trajectories_ggplot}()}.
#' 
#' @details
#' Posterior samples are extracted with memory monitoring. If the free memory 
#' falls bellow \code{reserve_mb}, the process is aborted with a message and 
#' \code{NULL} is returned.
#' 
#' The indicators are summarised from the following columns of the posterior 
#' samples:
#' \itemize{
#'   \item \code{"BB0"}: \code{BB0} (B/B0)
#'   \item \code{"BBmsy"}: \code{stock} (B/Bmsy)
#'   \item \code{"FFmsy"}: \code{harvest} (F/Fmsy)
#'   \item \code{"Bdev"}: \code{Bdev} (process deviations)
#'   \item \code{"B"}: \code{B} (biomass)
#'   \item \code{"H"}: \code{H} (harvest rate)
#'   \item \code{"Catch"}: \code{Catch} 
#'   \item \code{"BBfrac"}: \code{BBfrac} (B/Bfrac)
#'   \item \code{"Bref"}: \code{Bref} (biomass reference)
#' }
#'
#' @param list_fit_models Either a single fit returned by 
#'   \code{JABBA::fit_jabba()}, or a list of such fits (one per scenario).
#' @param reserve_mb A numeric value for the minimum free system memory, in 
#'   megabytes, to keep available. Defaults to 2048.
#' @param poll_interval A numeric value giving the time interval, in seconds, 
#'   between memory availability checks. Defaults to 0.5.
#'
#' @return An object of class \code{JAGGdata} (a data frame) with one row per 
#'   year, scenario and indicator, and the columns:
#' \itemize{
#'   \item \code{year}: Year of the observation.
#'   \item \code{Scenario}: Scenario name.
#'   \item \code{mu}: Median value.
#'   \item \code{lcl}: Lower 2.5% quantile.
#'   \item \code{ucl}: Upper 97.5% quantile.
#'   \item \code{lcl2}: Lower 10% quantile.
#'   \item \code{ucl2}: Upper 90% quantile.
#'   \item \code{indicator}: Name of the indicator summarised.
#' }
#' Returns \code{NULL} if the process is aborted due to low memory.
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
#' res1 <- trajectories_data(fit.S01)
#'
#' # A list of fits (one per scenario)
#' res2 <- trajectories_data(list(fit.S01, fit.S02))
#' }
#' 
#' @family preparation functions
#' @family trajectories functions
#' 
#' @export
#' @importFrom dplyr %>% bind_rows mutate rename summarise ungroup
#' @importFrom rlang .data
#' @importFrom stats median quantile
trajectories_data <- function(
  list_fit_models, reserve_mb = 2048, poll_interval = 0.5
) {
  # Posterior samples, aborting on low memory
  model_results <- tryCatch({
    .safe_execute(
      fn = function(kb) {
        .jbplot_ensemble2(
          kb = kb,
          kbout = TRUE,
          plot = FALSE
        )
      },
      args = list(
        kb = list_fit_models
      ),
      reserve_mb = reserve_mb,
      poll_interval = poll_interval
    )
  }, memoryLimitExceeded = function(e) {
    message("Process aborted to prevent the system from running out of memory")
    NULL
  })
  if (is.null(model_results)) return(invisible(NULL))

  # Indicator name -> column of the posterior samples
  columns <- list(
    BB0   = "BB0",
    BBmsy = "stock",
    FFmsy = "harvest",
    Bdev  = "Bdev",
    B = "B",
    H = "H",
    Catch = "Catch",
    BBfrac = "BBfrac",
    Bref = "Bref"
  )

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

  # Median and quantiles by year and scenario, for each indicator
  result_list <- lapply(names(columns), function(var_name) {
    var_col <- columns[[var_name]]
    model_results %>%
      summarise(
        mu   = median(.data[[var_col]]),
        lcl  = quantile(.data[[var_col]], probs = 0.025),
        ucl  = quantile(.data[[var_col]], probs = 0.975),
        lcl2 = quantile(.data[[var_col]], probs = 0.1),
        ucl2 = quantile(.data[[var_col]], probs = 0.9),
        indicator = var_name,
        .by = c(year, Scenario)
      )
  })

  results <- bind_rows(result_list) %>% 
    ungroup()

  class(results) <- c("JAGGdata", class(results))

  # Fail if the data frame is NA
  if (all(is.na(results))) {
    stop("All the data have NA values")
  }

  return(results)
}
