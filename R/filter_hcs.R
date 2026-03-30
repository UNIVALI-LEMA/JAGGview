filter_hcs <- function(object_list) {
  object_list[sapply(object_list, eh_hindcast_jabba)]
}