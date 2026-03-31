#' Format Numeric Values with Custom Decimal and Thousands Separators
#' 
#' @param number A numeric value (or a numeric vector of values) to be 
#' formated
#' @param decimals An integer indicating the number of decimals places. 
#' Defaults to 2.
#' @param big.mark An character used as the thousands separator. 
#' Defaults to ",".
#' @param decimal.mark An character used as the decimal separator. 
#' Defauts to ".".
#' 
#' @return A character value (or a character vector of values) with
#' formatted numbers
#' 
#' @examples
#' number <- 36.842105
#' format_number(number)
#' format_number(number, 1)
#' format_number(number, 1, ".", ",")
#' 
#' @export
format_number <- function(
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