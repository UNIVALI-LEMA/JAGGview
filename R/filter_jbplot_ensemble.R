filter_jbplot_ensemble <- function(object_list) {
  object_list[sapply(object_list, eh_jbplot_ensemble)]
}