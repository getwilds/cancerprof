#' Test Handle workforce
#'
#' This testthat file tests the handle-workforce function
test_that("handle workforce correctly maps workforce", {
  result <- sapply(c("unemployed"), handle_workforce)
  expected <- c(`unemployed` = "00012")

  expect_equal(result, expected)
})

test_that("handle workforce expects errors for incorrect arguments", {
  expect_error(handle_workforce("carrot"))
})
