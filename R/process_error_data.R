process_error_data <- function(list_models) {
  list_models %>%
    rename(Scenario = run) %>%
    mutate(year = as.integer(year)) %>%
    summarise(
      mu = median(Bdev),
      lcl = quantile(Bdev, probs = 0.025),
      ucl = quantile(Bdev, probs = 0.975),
      lcl2 = quantile(Bdev, probs = 0.1),
      ucl2 = quantile(Bdev, probs = 0.9),
      .by = c(year, Scenario)
    ) %>%
    ungroup()
}