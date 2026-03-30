filter_fits <- function(object_list) {
  object_list[sapply(object_list, is_fit_jabba)]
}