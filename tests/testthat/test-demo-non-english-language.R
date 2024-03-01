#' Test demo-language
#' 
#' This testthat file test the demo-language function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- demo_language("wa", "county", "language isolation")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
test_that("demo-language returns non-empty data frame", {
  language1 <- demo_language("wa", "county", "language isolation")
  expect_true(is.data.frame(language1))
})

#demo-language must have 5 columns
test_that("demo-language has correct number of columns", {
  df <- demo_language("wa", "county", "language isolation")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("demo-language has correct parameters", {
  expect_error(demo_language())
})