#' Test demo-education
#' 
#' This testthat file test the demo-education function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_education("wa", "county", "at least high school", "males")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
test_that("demo-education returns non-empty data frame", {
  skip_on_cran()
  education1 <- demo_education("wa", "county", "at least high school", "males")
  expect_true(is.data.frame(education1))
  
  education2 <- demo_education("ca", "hsa", "less than 9th grade")
  expect_true(is.data.frame(education2))
  
  education3 <- demo_education("usa", "state", "at least bachelors degree", 
                               "both sexes", "all races (includes hispanic)")
  expect_true(is.data.frame(education3))
})

#demo-education must have 5 columns
test_that("demo-education has correct number of columns", {
  skip_on_cran()
  df <- demo_education("wa", "county", "at least high school", "both sexes")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#test error handling

#write one for error handling in education - mention in the message how values could have chnaged in SCP
test_that("demo-education handles invalid education parameters", {
  skip_on_cran()
  expect_error(
    demo_education("wa", "county", "less than 9th grade", "males"),
    "For Less than 9th Grade, Race and Sex must be NULL"
  )
  expect_error(
    demo_education("wa", "county", "at least high school", "all races (includes hispanic", "females"),
    "For At Least High School, Race must be NULL and Sex must be NOT NULL"
  )
  expect_error(
    demo_education("wa", "county", "at least bachelors degree", "both sexes"),
    "For At Least Bachelors Degree, Race and Sex must be NOT NULL."
  )
})

#parameter
test_that("demo-education has correct parameters", {
  expect_error(demo_education())
})