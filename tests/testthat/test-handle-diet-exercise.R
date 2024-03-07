#' Test Handle diet_exercise
#'
#' This testthat file tests the handle-diet_exercise function
test_that("handle diet_exercise correctly maps diet_exercise", {
  result <- sapply(c(
    "bmi is healthy, ages 20+", "bmi is obese, ages 20+",
    "consumed 1 or more fruits per day"
  ), handle_diet_exercise)
  expected <- c(
    `bmi is healthy, ages 20+` = "v01", `bmi is obese, ages 20+` = "v02",
    `consumed 1 or more fruits per day` = "v50"
  )

  expect_equal(result, expected)
})

test_that("handle diet_exercise expects errors for incorrect arguments", {
  expect_error(handle_diet_exercise("carrot"))
})
