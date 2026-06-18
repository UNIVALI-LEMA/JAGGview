test_that(".array_to_dataframe() converts 3D array to data frame correctly", {
  arr <- array(1:8, dim = c(2, 2, 2))
  dimnames(arr) <- list(
    c("2000", "2001"),
    c("A", "B"),
    c("idx1", "idx2")
  )
  
  result <- .array_to_dataframe(arr)
  
  expect_true(is.data.frame(result))
  expect_true(all(c("Index", "Year") %in% names(result)))
  expect_equal(nrow(result), 4)
})

test_that(".array_to_dataframe() assigns correct Index values", {
  arr <- array(1:8, dim = c(2, 2, 2))
  dimnames(arr) <- list(
    c("2000", "2001"),
    c("A", "B"),
    c("idx1", "idx2")
  )
  
  result <- .array_to_dataframe(arr)
  
  expect_true(all(result$Index %in% c("idx1", "idx2")))
})

test_that(".array_to_dataframe() throws error for non-3D input", {
  arr <- matrix(1:4, nrow = 2)
  
  expect_error(.array_to_dataframe(arr))
})

test_that(".replace_na_with_na() replaces values based on NA pattern", {
  df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  df2 <- data.frame(a = c(NA, 2, NA), b = c(4, NA, 6))
  
  result <- .replace_na_with_na(df1, df2)
  
  expect_true(is.na(result$a[1]))
  expect_true(is.na(result$a[3]))
  expect_true(is.na(result$b[2]))
})

test_that(".replace_na_with_na() preserves non-NA values", {
  df1 <- data.frame(a = c(1, 2, 3))
  df2 <- data.frame(a = c(NA, 2, NA))
  
  result <- .replace_na_with_na(df1, df2)
  
  expect_equal(result$a[2], 2)
})

test_that(".rename_columns() correctly renames columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  result <- .rename_columns(df, c("x", "y"))
  
  expect_equal(names(result), c("x", "y"))
})

test_that(".rename_columns() keeps data unchanged", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  result <- .rename_columns(df, c("x", "y"))
  
  expect_equal(result$x, df$a)
  expect_equal(result$y, df$b)
})