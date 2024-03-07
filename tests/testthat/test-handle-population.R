#' Test Handle population
#'
#' This testthat file tests the handle-population function
test_that("handle population correctly maps population", {
  result <- sapply(c("age under 18", "foreign born", "males"), handle_population)
  expected <- c(`age under 18` = "00002", `foreign born` = "00014", `males` = "00104")

  expect_equal(result, expected)
})

test_that("handle population expects errors for incorrect arguments", {
  expect_error(handle_population("carrot"))
})
