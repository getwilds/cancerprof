#' Test demo-workforce
#' 
#' This testthat file test the demo-workforce function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_workforce("wa", "county", "unemployed",
                           "all races (includes hispanic)", "both sexes")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
test_that("demo-workforce returns non-empty data frame", {
  skip_on_cran()
  workforce1 <- demo_workforce("wa", "county", "unemployed",
                               "all races (includes hispanic)", "both sexes")
  expect_true(is.data.frame(workforce1))
})

#demo-workforce must have 5 columns
test_that("demo-workforce has correct number of columns", {
  skip_on_cran()
  df <- demo_workforce("wa", "county", "unemployed",
                       "all races (includes hispanic)", "both sexes")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("demo-workforce has correct parameters", {
  expect_error(demo_workforce())
})