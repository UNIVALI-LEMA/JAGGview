#' International System of Prefixes
#' 
#' @param number A numeric value (or a numeric vector of values) to be 
#' formated using SI prefixes.
#' @param decimals An integer indicating the number of decimals places
#' to display in the formatted string. Defaults to 2. 
#' 
#' @return A character value (or a character vector of values) 
#' appended with their corresponding SI unit symbol.
#' 
#' @examples 
#' number <- 1000000
#' international_system_prefixes(number, 2)
#' 
#' @export
international_system_prefixes <- function(number, decimals = 2) {
  value <- ifelse(
    abs(number) >= 1e6, number / 1e6,
    ifelse(
      abs(number) >= 1e3, number / 1e3,
      ifelse(
        abs(number) >= 1, number,
        ifelse(
          abs(number) >= 1e-3, number * 1e3,
          ifelse(
            abs(number) >= 1e-6, number * 1e6,
            number * 1e9
          )
        )
      )
    )
  )
  
  suffix <- ifelse(
    abs(number) >= 1e6, "M",
    ifelse(
      abs(number) >= 1e3, "k",
      ifelse(
        abs(number) >= 1, "",
        ifelse(
          abs(number) >= 1e-3, "m",
          ifelse(
            abs(number) >= 1e-6, "\u00B5",
            "n"
          )
        )
      )
    )
  )
  
  paste0(trimws(.format_number(value, decimals = decimals)), suffix)
}

.format_number <- function(
  number, decimals = 2, big.mark = ",", decimal.mark = "."
) {
  format(
    round(number, decimals),
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    nsmall = decimals,
    scientific = FALSE
  )
}