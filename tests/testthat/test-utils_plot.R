test_that(
  desc = ".axis_limit() validates the axis limits", 
  code = {
    expect_equal(.axis_limit(c(0, 100)), NULL)
    expect_equal(.axis_limit(c(10, 1e6)), NULL)
    expect_equal(.axis_limit(c(1950, 2026)), NULL)
    expect_equal(.axis_limit(NULL), NULL)
  }
)

test_that(
  desc = ".axis_limit() invalidates the vector",
  code = {
    expect_error(.axis_limit(1))
    expect_error(.axis_limit(c(1e3, 0)))
    expect_error(.axis_limit(c("1970", "2020")))
    expect_error(.axis_limit(c(NA, 2000)))

  }
)

test_that(
  desc = ".default_table mantain the correct structure",
  code = {
    df <- data.frame(
      name = c("a", "b"),
      value = c(0, 3)
    )
    type_name <- class(df$name)
    table <- .default_table(df)
    expect_s3_class(table, c("gt_tbl", "list"))

    expect_equal(nrow(table$`_data`), nrow(df))
    expect_equal(ncol(table$`_data`), ncol(df))
    expect_equal(table$`_data`$name, df$name)
    expect_equal(table$`_data`$value, df$value)
    expect_type(table$`_data`$name, type_name)
    expect_type(table$`_data`$value, "double")
  }
)

test_that(
  desc = ".default_table cause errors with inputs",
  code = {
    expect_error(
      .default_table(data.frame(name = c("a", "b", "c"), value = c(1, 6)))
    )
    expect_error(.default_table(NULL))
    expect_error(.default_table(data.frame(name = numeric(0), value = numeric(0))))
  }
)

test_that(".international_system_prefixes(), returns the number 
formatted and appended with their corresponding SI unit symbol", {
  expect_equal(.international_system_prefixes(1000000), "1M")
})

test_that(
  ".international_system_prefixes(), error if inital number is a character", 
  {
    expect_error(.international_system_prefixes("a"))
  }
)
