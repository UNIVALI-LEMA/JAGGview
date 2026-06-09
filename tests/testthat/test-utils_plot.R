test_that(".international_system_prefixes(), returns the number 
formatted and appended with their corresponding SI unit symbol", {
  expect_equal(.international_system_prefixes(1000000), "1M")
})

test_that(".international_system_prefixes(), error if inital number 
is a character", {
  expect_error(.international_system_prefixes("a"))
})
