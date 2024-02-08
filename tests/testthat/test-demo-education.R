#' Test demo-education
#' 
#' This testthat file test the demo-education function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- demo_crowding("usa", "state", "All Races (includes Hispanic)")
  
  expect_equal(class(output), "data.frame",
               info = "Output should be a data frame")
  
  expect_equal(typeof(output), "list",
               info = "Output should have list storage type (since data frames are lists)")
})

#Ensures that variables are present and working on SCP
test_that("demo-education returns non-empty data frame", {
  education1 <- demo_education("wa", "county", "at least high school", "males")
  expect_true(!is.null(education1))
  expect_true(is.data.frame(education1))
  
  education2 <- demo_education("ca", "hsa", "less than 9th grade")
  expect_true(!is.null(education2))
  expect_true(is.data.frame(education2))
  
  education3 <- demo_education("usa", "state", "at least bachelors degree", 
                               "both sexes", "all races (includes hispanic)")
  expect_true(!is.null(education3))
  expect_true(is.data.frame(education3))
})

#demo-education must have 5 columns
test_that("demo-education has correct number of columns", {
  df <- demo_education("wa", "county", "at least high school", "both sexes")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#test error handling
test_that("demo-education handles invalid education parameters", {
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

# What expected data would I be comparing too?

# test_that("demo-education returns the expected columns", {
#   expect_identical(names(education2), names(expected_data))
# })
