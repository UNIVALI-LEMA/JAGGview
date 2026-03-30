round_up_to_nearest <- function(value) {
  magnitude <- 10^(floor(log10(value)))
  rounded_value <- ceiling(value / magnitude) * magnitude
  return(rounded_value)
}