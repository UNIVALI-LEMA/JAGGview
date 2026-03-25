si_prefixes <- function(number, decimals = 2) {
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
            abs(number) >= 1e-6, "µ",
            "n"
          )
        )
      )
    )
  )
  
  paste0(trimws(format_number(value, decimals = decimals)), suffix)
}