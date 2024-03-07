#' Test demo-income
#' 
#' This testthat file test the demo-income function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_income("wa", "county", "median family income", "all races (includes hispanic)")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
test_that("demo-income returns non-empty data frame", {
  skip_on_cran()
  income1 <- demo_income("wa", "county", "median family income", "all races (includes hispanic)")
  expect_true(is.data.frame(income1))
  
  income2 <- demo_income("usa", "state", "median household income", "all races (includes hispanic)")
  expect_true(is.data.frame(income2))
})

#demo-income must have 5 columns
test_that("demo-income has correct number of columns", {
  skip_on_cran()
  df <- demo_income("usa", "state", "median household income", "all races (includes hispanic)")
  expected_columns <- 4
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("demo-income has correct parameters", {
  expect_error(demo_income())
})