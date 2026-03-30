replace_na_with_na <- function(df1, df2) {
  df1[] <- lapply(names(df1), function(col) {
    replace(df1[[col]], is.na(df2[[col]]), NA)
  })
  return(df1)
}