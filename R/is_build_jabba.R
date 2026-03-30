is_build_jabba <- function(obj) {
  is.list(obj) && all(c("data", "jagsdata", "settings") %in% names(obj))
}