#' Test Handle food
#'
#' This testthat file tests the handle-food function
test_that("handle food correctly maps food", {
  result <- sapply(c("food insecurity", "limited access to healthy food"), handle_food)
  expected <- c(`food insecurity` = "03003", `limited access to healthy food` = "03004")

  expect_equal(result, expected)
})

test_that("handle food expects errors for incorrect arguments", {
  expect_error(handle_food("carrot"))
})
