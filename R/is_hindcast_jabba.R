is_hindcast_jabba <- function(obj) {
  if(!is.list(obj)) return(FALSE)

  if (length(obj) == 0) return(FALSE)
  
  elem <- obj[[1]]

  is_fit_jabba(elem)
}