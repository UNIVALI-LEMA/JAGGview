cpue_conflicts_data <- function(list_models) {
  process_stats(list_models) %>%
    mutate(x = 2015, y = 0.8) %>%
    filter(Stastistic == "RMSE")
}