#' Test Handle income
#'
#' This testthat file tests the handle-income function
test_that("handle income correctly maps income", {
  result <- sapply(c("median family income", "median household income"), handle_income)
  expected <- c(`median family income` = "00010", `median household income` = "00011")

  expect_equal(result, expected)
})

test_that("handle income expects errors for incorrect arguments", {
  expect_error(handle_income("carrot"))
})
