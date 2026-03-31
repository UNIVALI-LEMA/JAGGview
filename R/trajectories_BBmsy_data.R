trajectories_BBmsy_data <- function(list_models) {
  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(stock),
      lcl = quantile(stock, probs = 0.025),
      ucl = quantile(stock, probs = 0.975),
      lcl2 = quantile(stock, probs = 0.1),
      ucl2 = quantile(stock, probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}