format_number <- function(n, decimals=2, big.mark = ",", decimal.mark = "."){
  format(
    round(n, decimals),
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    nsmall = decimals,
    scientific = FALSE
  )
}