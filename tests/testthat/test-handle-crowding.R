#' Test Handle crowding
#'
#' This testthat file tests the handle-crowding function
test_that("handle crowding correctly maps crowding", {
  result <- sapply(c("household with >1 person per room"), handle_crowding)
  expected <- c(`household with >1 person per room` = "00027")

  expect_equal(result, expected)
})

test_that("handle crowding expects errors for incorrect arguments", {
  expect_error(handle_crowding("carrot"))
})
