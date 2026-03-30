filter_jbs <- function(object_list) {
  object_list[sapply(object_list, is_build_jabba)]
}