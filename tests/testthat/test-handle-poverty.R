#' Test Handle poverty
#'
#' This testthat file tests the handle-poverty function
test_that("handle poverty correctly maps poverty", {
  result <- sapply(c(
    "families below poverty", "persistent poverty",
    "persons below poverty"
  ), handle_poverty)
  expected <- c(
    `families below poverty` = "00007", `persistent poverty` = "03001",
    `persons below poverty` = "00008"
  )

  expect_equal(result, expected)
})

test_that("handle poverty expects errors for incorrect arguments", {
  expect_error(handle_poverty("carrot"))
})
