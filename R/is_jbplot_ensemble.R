is_jbplot_ensemble <- function(obj) {
  is.data.frame(obj) && all(c(
    "year", "run", "stock",
    "harvest", "B", "H",
    "Bdev", "Catch", "BB0",
    "BBfrac", "Bref"
  ) %in% names(obj))
}