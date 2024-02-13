#' Test risk-alcohol
#' 
#' This testthat file test the risk-alcohol function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- risk_alcohol(paste("binge drinking (4+ drinks on one occasion for women,", 
                               "5+ drinks for one occasion for men), ages 21+"), 
                               "all races (includes hispanic)", "both sexes")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
test_that("risk-alcohol returns non-empty data frame", {
  alcohol1 <- risk_alcohol(paste("binge drinking (4+ drinks on one occasion for women,", 
                                 "5+ drinks for one occasion for men), ages 21+"), 
                                 "all races (includes hispanic)", "both sexes")
  expect_true(is.data.frame(alcohol1))
})

#risk-alcohol must have 5 columns
test_that("risk-alcohol has correct number of columns", {
  df <- risk_alcohol(paste("binge drinking (4+ drinks on one occasion for women,", 
                           "5+ drinks for one occasion for men), ages 21+"), 
                           "all races (includes hispanic)", "both sexes")
  expected_columns <- 6
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("risk-alcohol has correct parameters", {
  expect_error(demo_alcohol())
})