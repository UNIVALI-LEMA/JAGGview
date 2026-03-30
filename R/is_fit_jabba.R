id_fit_jabba <- function(obj) {
  is.list(obj) && all(c("estimates","timeseries", "posteriors", "kobe") %in% names(obj))
}