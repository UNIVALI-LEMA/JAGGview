trajectories_FFmsy_data <- function(list_models) {
  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(harvest),
      lcl = quantile(harvest, probs = 0.025),
      ucl = quantile(harvest, probs = 0.975),
      lcl2 = quantile(harvest, probs = 0.1),
      ucl2 = quantile(harvest, probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}