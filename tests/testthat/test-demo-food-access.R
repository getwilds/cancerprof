#' Test Food Access
#'
#' This testthat file test the demo-food function

# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_food("wa", "county", "food insecurity", "black")

  expect_equal(class(output), "data.frame",
    info = "Output should be a data frame"
  )

  expect_equal(typeof(output), "list",
    info = "Output should have list storage type (since data frames are lists)"
  )
})

# Ensures that variables are present and working on SCP
test_that("demo-food returns non-empty data frame", {
  skip_on_cran()
  food_test_1 <- demo_food("wa", "county", "food insecurity", "all races (includes hispanic)")
  expect_true(!is.null(food_test_1))
  expect_true(is.data.frame(food_test_1))

  food_test_2 <- demo_food("usa", "state", "limited access to healthy food")
  expect_true(!is.null(food_test_2))
  expect_true(is.data.frame(food_test_2))

  food_test_3 <- demo_food("pr", "county", "food insecurity", "all races (includes hispanic)")
  expect_true(!is.null(food_test_3))
  expect_true(is.data.frame(food_test_3))
})

# sometimes functions will output different nuber of columns depending on variables...
# demo-food must have 5 columns
test_that("demo-food has correct number of columns", {
  skip_on_cran()
  df <- demo_food("wa", "county", "food insecurity", "black")
  df2 <- demo_food("wa", "county", "limited access to healthy food")
  expected_columns <- 3
  expected_columns2 <- 4
  expect_equal(ncol(df), expected_columns)
  expect_equal(ncol(df2), expected_columns2)
})

# test error handling
test_that("demo-food handles invalid education parameters", {
  skip_on_cran()
  expect_error(
    demo_food("wa", "county", "limited access to healthy food", "all races (includes hispanic)"),
    "For limited access to healthy food, Race must be NULL"
  )
  expect_error(
    demo_food("wa", "county", "food insecurity"),
    "For food insecurity, Race must NOT be NULL."
  )
})

# parameter
test_that("demo-food has correct parameters", {
  expect_error(demo_food())
})
