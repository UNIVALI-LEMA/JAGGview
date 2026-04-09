#' Convert a 3D array into a long-format data frame
#'
#' Internal helper that transforms a three-dimensional array into a
#' data frame by stacking slices along the third dimension and adding
#' index and year columns.
#'
#' @param arr A three-dimensional array.
#'
#' @return A data frame with combined slices and additional columns
#'   identifying the index and year.
#'
#' @keywords internal
.array_to_dataframe <- function(arr) {
  if (length(dim(arr)) != 3) {
    stop("The input must be a three-dimensional array.")
  }
  df_list <- list()
  dims <- dim(arr)
  for (k in 1:dims[3]) {
    slice <- arr[,,k]
    df <- as.data.frame(slice)
    df$Index <- dimnames(arr)[[3]][k]
    df$Year <- as.numeric(row.names(df))
    df_list[[k]] <- df
  }
  result <- do.call(rbind, df_list)
  return(result)
}

#' Replace values based on NA pattern of another data frame
#'
#' Internal helper that replaces values in one data frame with NA
#' wherever the corresponding positions in another data frame are NA.
#'
#' @param df1 A data frame to be modified.
#' @param df2 A data frame providing the NA pattern.
#'
#' @return A data frame with values replaced by NA where applicable.
#'
#' @keywords internal
.replace_na_with_na <- function(df1, df2) {
  df1[] <- lapply(names(df1), function(col) {
    replace(df1[[col]], is.na(df2[[col]]), NA)
  })
  return(df1)
}

