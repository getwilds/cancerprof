#' Test demo-crowding
#'
#' This testthat file test the demo-crowding function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_crowding(
    area = "wa",
    areatype = "hsa",
    crowding = "household with >1 person per room",
    race = "All Races (includes Hispanic)"
  )

  expect_true(inherits(output, "data.frame"))
})

# Ensures that variables are present and working on SCP
test_that("demo-crowding returns non-empty data frame", {
  skip_on_cran()
  crowding1 <- demo_crowding(
    area = "wa",
    areatype = "hsa",
    crowding = "household with >1 person per room",
    race = "All Races (includes Hispanic)"
  )
  expect_true(is.data.frame(crowding1))
})

# demo-crowding must have 5 columns
test_that("demo-crowding has correct number of columns", {
  skip_on_cran()
  df <- demo_crowding(
    area = "wa",
    areatype = "hsa",
    crowding = "household with >1 person per room",
    race = "All Races (includes Hispanic)"
  )
  expected_columns <- 5

  expect_equal(ncol(df), expected_columns)
})

# parameter
test_that("demo-crowding has correct parameters", {
  expect_error(demo_crowding())
})
