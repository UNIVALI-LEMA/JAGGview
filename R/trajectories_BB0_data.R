trajectories_BB0_data <- function(list_models) {
  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(BB0),
      lcl = quantile(BB0, probs = 0.025),
      ucl = quantile(BB0, probs = 0.975),
      lcl2 = quantile(BB0, probs = 0.1),
      ucl2 = quantile(BB0, probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}