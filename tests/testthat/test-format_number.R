test_that("format_number(), return the number formatted with 
the chosen decimals", {
  expect_equal(format_number(36.842105), "36.84")
})

test_that("format_number(), error if inital number is a character", {
  expect_error(format_number("a"))
})