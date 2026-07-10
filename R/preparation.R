#' Prepare fitted index data and credibility intervals
#'
#' Processes model outputs to generate formatted data for fitted indices_factor,
#' including mean values and credibility intervals (80% and 95%).
#'
#' Extracts index and uncertainty information from a list of model results, 
#' computes upper and lower credibility bounds, and organizes the data for 
#' downstream visualization.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#'   JABBA function \code{JABBA::fit_jabba()}.
#' @param indices_factor Optional. A vector of indices_factor to include. Must 
#'   exist in the \code{Index} column.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{Li_Ui}{A data frame containing mean values and lower (Li) and
#'   upper (Ui) credibility bounds.}
#'   \item{CI_80}{A data frame containing fitted values and 80% credibility
#'   intervals.}
#'   \item{CI_95}{A data frame containing fitted values and 95% credibility
#'   intervals.}
#' }
#'
#' @details
#' The function processes scenario-based outputs for indices_factor (\code{I}),
#' standard errors (\code{SE2}), and fitted values (\code{ppd} and \code{hat}).
#' Missing values are handled using reference index data when necessary.
#'
#' Credibility intervals for \code{Li_Ui} are computed assuming normality,
#' using a multiplier of 1.96 applied to the standard error.
#'
#' If \code{indices_factor} is provided, the results are filtered and reordered
#' accordingly. The function also ensures consistency across different
#' model outputs and removes incomplete cases before returning results.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' result <- fits_data(list_fit_models)
#' result
#' }
#'
#' @export
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr %>% select filter full_join 
#' @importFrom stats complete.cases
#' @importFrom forcats fct_relevel
fits_data <- function(list_fit_models, indices_factor = NULL) {
  # ###@> Filtering the expected data...
  # .validate_fits_input_data(list_fit_models)
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  ###@> Index...
  tmp01 <- .process_scenarios(list_fit_models, vars = "I")
  
  ###@> SE...
  tmp02 <- .process_scenarios(list_fit_models, vars = "SE2")

  ##@> Replacing NA...
  tmp02 <- .replace_na_with_na(tmp02, tmp01)

  ####@> Merging tmp01 and tmp02...
  tmp01 <- pivot_longer(
    data = tmp01, names_to = "Index", values_to = "Mean", 3:ncol(tmp01)
  )
  tmp02 <- pivot_longer(
    data = tmp02, names_to = "Index", values_to = "SE", 3:ncol(tmp02)
  )
  tmp00 <- full_join(tmp01, tmp02)

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp00$Index), indices_factor)
  }

  ####@> Estimating Upper and Lower errors...
  tmp00$error <- with(tmp00, (1.96 * sqrt(SE)))
  tmp00$Ui <- with(tmp00, Mean + error)
  tmp00$Li <- with(tmp00, Mean - error)

  index_inputseries <- .process_index(list_fit_models)

  if (any(is.na(tmp00$Index))) .fill_na_indices(tmp00, index_inputseries)

  tmp00 <- tmp00[complete.cases(tmp00),] %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>%
    select(-SE)

  ####@> Fit (CI 80%)...
  tmp03 <- .process_cpues(list_fit_models, vars = "ppd")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp03$Index), indices_factor)
  }

  if (any(is.na(tmp03$Index))) .fill_na_indices(tmp03, index_inputseries)

  tmp03 <- tmp03 %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>% 
    select(-c(se, obserror))

  ####@> Fit (CI 95%)...
  tmp04 <- .process_cpues(list_fit_models, vars = "hat")

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp04$Index), indices_factor)
  }

  if (any(is.na(tmp04$Index))) .fill_na_indices(tmp04, index_inputseries)

  tmp04 <- tmp04 %>% 
    mutate(Index = fct_relevel(Index, indices_factor)) %>%
    select(-c(se, obserror, mu))

  results <- list(
    Li_Ui = tmp00,
    CI_80 = tmp03,
    CI_95 = tmp04
  )
  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }
  
  return(results)
}

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

#' Prepare data for Kobe plot visualization
#'
#' Computes biomass and fishing mortality ratios and prepares all required 
#' components to build a Kobe plot, including credibility contours and 
#' reference quadrants.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#' \code{harvest} (F/Fmsy), and \code{stock} (B/Bmsy), returned by the JABBA 
#' function \code{JABBA::jbplot_ensemble()}.
#' @param ci_levels A numeric vector containing the CI values between 0 and 1.
#' Deafults to 0.5, 0.8, 0.95
#'
#' @return A named list containing:
#' \describe{
#'   \item{col01}{Data frame defining the yellow Kobe quadrant (overfished, 
#'   not overfishing).}
#'   \item{col02}{Data frame defining the orange quadrant (overfished and 
#'   overfishing).}
#'   \item{col03}{Data frame defining the red quadrant (severely 
#'   overfished and overfishing).}
#'   \item{col04}{Data frame defining the green quadrant (healthy stock 
#'   conditions).}
#'   \item{k.out}{Data frame with kernel density contours (50%, 80%, 95%) 
#'   for the terminal year.}
#'   \item{tmp11}{Time series of median biomass and fishing mortality 
#'   ratios by year and scenario.}
#'   \item{tmp11b}{Subset of selected years (e.g., 1950, 1986, 2023) for 
#'   highlighting points.}
#' }
#'
#' @details
#' The function computes median biomass (B/Bmsy) and fishing mortality (F/Fmsy) 
#' ratios by year and scenario, and estimates kernel density contours for the 
#' terminal year using \code{gplots::ci2d}. These contours represent 
#' uncertainty regions commonly displayed in Kobe plots.
#'
#' The output is designed to be used directly with \code{kobe_ggplot()}.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- kobe_data(list_fit_models)
#' str(df)
#' }
#'
#' @export
#' @importFrom dplyr %>% summarise arrange filter
#' @importFrom stats median
#' @importFrom gplots ci2d
kobe_data <- function(list_fit_models, ci_levels = c(0.5, 0.8, 0.95)) {
  # ###@> Filtering the expected data...
  # .validate_jbplot_ensemble(model_results)
  # model_results <- tryCatch({
  #   .jbplot_ensemble2(
  #     kb = list_fit_models,
  #     kbout = TRUE,
  #     plot = FALSE
  #   )
  # }, error = function(e) {
  #   message("An error ocurred: ", e$message)
  #   return(NULL)
  # })
  model_results <- tryCatch({
    .safe_execute(
      fn = function() .jbplot_ensemble2(
        kb = list_fit_models, kbout = TRUE, plot = FALSE
      )
    )
  }, memoryLimitExceeded = function(e) {
    message("Process aborted for security, before crashing the system.")
    NULL
  })


  if (!inherits(ci_levels, "numeric")) {
    stop("Parameter 'ci_levels' was expecting a numeric vector")
  }

  if (any(is.na(ci_levels))) {
    stop("Parameter 'ci_levels' cannot contain NA.")
  }

  if (any(ci_levels <= 0 | ci_levels >= 1)) {
    stop("Parameter 'ci_levels' was expecting numbers between 0 and 1.")
  }

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

  #####@> Extracting data...
  tmp11 <- model_results %>%
    summarise(
      Fratio = median(harvest),
      Bratio = median(stock),
      .by = c(year, Scenario)
    ) %>%
    arrange(Scenario, year)

  max_x <- ceiling(max(tmp11$Bratio))
  max_y <- ceiling(max(tmp11$Fratio))
  
  col01 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(0, 0), ymax = c(1, 1), 
    col = "yellow"
  )
  col02 <- data.frame(
    xmin = c(1, 1), xmax = c(max_x, max_x), 
    ymin = c(1, 1), ymax = c(max_y, max_y), 
    col = "orange"
  )
  col03 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(1, 1), ymax = c(max_y, max_y), 
    col = "red"
  )
  col04 <- data.frame(
    xmin = c(1, 1), xmax = c(max_x, max_x), ymin = c(0, 0), ymax = c(1, 1), 
    col = "#00FF00"
  )

  max_year <- max(model_results$year)
  min_year <- min(model_results$year)
  mid_year <- round(min_year + (max_year - min_year)/2)

  tmp11b <- filter(tmp11, year %in% c(min_year, mid_year, max_year))
  tmp11c <- filter(model_results, year == max_year)

  k.out <- data.frame(x = NULL, y = NULL, Scenario = NULL, q = NULL)
  for(i in unique(model_results$Scenario)) {
    x <- filter(model_results, Scenario == i)
    x <- filter(x, year == max_year)
    kernelF <- ci2d(
      x$stock, 
      x$harvest, 
      nbins = 151,  # See if can be generic (seems more of a parameter)
      factor = 1.5, # See if can be generic (seems more of a parameter)
      ci.levels = ci_levels,
      show = "none",
      col = 1       # See if can be generic 
    )

    tmp00 <- lapply(
      ci_levels, function(ci) {
        q <- kernelF$contours[[as.character(ci)]]
        q$Scenario <- i
        q$q <- paste0(ci*100, "%")
        q
      })
    
    tmp <- do.call(rbind, tmp00)

    k.out <- rbind(
      k.out, 
      data.frame(
        x = tmp$x,
        y = tmp$y,
        Scenario = tmp$Scenario,
        q = fct_relevel(tmp$q, sort(unique(tmp$q), decreasing = TRUE))
      )
    )
  }
  
  results <- list(
    col01 = col01[1, , drop = FALSE],
    col02 = col02[1, , drop = FALSE],
    col03 = col03[1, , drop = FALSE],
    col04 = col04[1, , drop = FALSE],
    k.out = k.out,
    tmp11 = tmp11,
    tmp11b = tmp11b
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(is.na(results))) {
    stop("Data frame only have NA data.")
  }
  return(results)
}

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

#' Prepare runs test diagnostics data
#'
#' Processes model outputs to compute runs test diagnostics and residual
#' structures for CPUE indices, including credibility limits and LOESS
#' smoothing.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#'   JABBA function \code{JABBA::fit_jabba()}.
#' @param indices_factor Optional. A vector of indices to include. Must exist
#'   in the \code{Index} column.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{cpue_residuals}{A data frame containing residuals, fitted
#'   LOESS values, and credibility bands.}
#'   \item{SE3}{A data frame containing runs test results, including
#'   lower and upper credibility limits and p-values.}
#'   \item{RMSE_data}{A data frame with RMSE-related diagnostics.}
#' }
#'
#' @details
#' The function computes runs tests using \code{JABBA::jbruns_sig3} and 
#' classifies results based on statistical significance. Residuals are smoothed 
#' using LOESS, and credibility intervals are derived from the fitted model.
#' 
#' If \code{indices_factor} is provided, the results are filtered and reordered
#' accordingly. The function also ensures consistency across different model 
#' outputs and removes incomplete cases before returning results.
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- runs_tests_data(list_fit_models)
#' df
#' }
#'
#' @export
#' @importFrom dplyr %>% mutate filter left_join select
#' @importFrom JABBA jbruns_sig3
#' @importFrom stats loess predict complete.cases
#' @importFrom forcats fct_relevel
runs_tests_data <- function(list_fit_models, indices_factor = NULL) {
  # ###@> Filtering the expected data...
  # .validate_fits_input_data(list_fit_models)
  if (.is_fit_jabba(list_fit_models)) {
    list_fit_models <- list(list_fit_models)
  }

  tmp05 <- .process_runs(list_fit_models)

  indices <- 4:ncol(tmp05)

  min_year <- min(tmp05$Year, na.rm = TRUE)

  max_year <- max(tmp05$Year, na.rm = TRUE)

  #####@> Runstest...
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
      index <- tmp05[tmp05$Scenario == j, i]
      index <- index[complete.cases(index)]
      test <- jbruns_sig3(index, type = "resid")
      out.test$lcl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[1]
      out.test$ucl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[2]
      out.test$pvalue[out.test$Index == name &
                      out.test$Scenario == j] <- test$p.runs
    }
  }
  out.test$class <- ifelse(out.test$pvalue < 0.05, "red", "green")
  out.test <- out.test[complete.cases(out.test),]

  ####@> Pivoting table...
  tmp05 <- pivot_longer(
    tmp05, names_to = "Index", values_to = "Res", indices
  ) 

  if (!is.null(indices_factor)) {
    .validate_indices(unique(tmp05$Index), indices_factor)
  }
  
  tmp05 <- tmp05 %>%
    filter(complete.cases(.)) %>%
    left_join(out.test, by = c("Scenario", "Index")) %>%
    select(Year:Res, lcl, ucl) %>%
    mutate(
      class = ifelse(Res < lcl | Res > ucl, "red", "white"),
      Index = fct_relevel(Index, indices_factor)
    ) %>%
    droplevels()
  
  loess_fit <- loess(Res ~ Year, data = tmp05)
  tmp05$fit <- predict(loess_fit)
  pred <- predict(loess_fit, se = TRUE)

  tmp05$fit   <- pred$fit
  tmp05$upper <- pred$fit + 1.96 * pred$se.fit
  tmp05$lower <- pred$fit - 1.96 * pred$se.fit

  RMSE_data <- .process_stats(list_fit_models) %>%
    filter(Stastistic == "RMSE")

  results <- list(
    cpue_residuals = tmp05,
    SE3 = out.test,
    RMSE_data = RMSE_data
  )

  class(results) <- c("JAGGdata", class(results))

  if (all(sapply(results, function(df) all(is.na(df))))) {
    stop("All the data frames have NA data.")
  }

  return(results)
}

#' Summarise trajectory data from model outputs
#'
#' Computes summary statistics (median and quantiles) selected variables from 
#' model outputs, grouped by year and scenario.
#'
#' @param list_fit_models A list containing model outputs as returned by the 
#' JABBA function \code{JABBA::fit_jabba()}.
#'
#' @return A data.frame containing:
#' \itemize{
#'   \item \code{year}: Year of the observation
#'   \item \code{Scenario}: Scenario name
#'   \item \code{mu}: Median value
#'   \item \code{lcl}: Lower 2.5% quantile
#'   \item \code{ucl}: Upper 97.5% quantile
#'   \item \code{lcl2}: Lower 10% quantile
#'   \item \code{ucl2}: Upper 90% quantile
#'   \item \code{indicator}: Name of the indicator summarised
#' }
#'
#' @details
#' The function maps user-friendly indicator_name names to:
#' \itemize{
#'   \item \code{"BB0"}: \code{BB0}
#'   \item \code{"BBmsy"}: \code{stock}
#'   \item \code{"FFmsy"}: \code{harvest}
#' }
#'
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' df <- trajectories_data(list_fit_models)
#' df
#' }
#' 
#' @export
#' @importFrom dplyr %>% rename mutate summarise bind_rows ungroup
#' @importFrom stats median quantile
trajectories_data <- function(list_fit_models) {
  # ###@> Filtering the expected data...
  # .validate_jbplot_ensemble(model_results)
  model_results <- .jbplot_ensemble2(
    kb = list_fit_models,
    kbout = TRUE,
    plot = FALSE
  )

  columns <- list(
    BB0   = "BB0",
    BBmsy = "stock",
    FFmsy = "harvest",
    Bdev  = "Bdev",
    B = "B", # This and below are in development
    H = "H",
    Catch = "Catch",
    BBfrac = "BBfrac",
    Bref = "Bref"
  )

  model_results <- model_results %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year))

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

  if (all(is.na(results))) {
    stop("Data frame only have NA data.")
  }

  return(results)
}