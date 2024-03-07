#' Test Handle stage
#'
#' This testthat file tests the handle-stage function
test_that("handle stage correctly maps stage", {
  result <- sapply(c("all stages", "late stage (regional & distant)"), handle_stage)
  expected <- c(`all stages` = "999", `late stage (regional & distant)` = "211")

  expect_equal(result, expected)
})

test_that("handle stage expects errors for incorrect arguments", {
  expect_error(handle_stage("carrot"))
})
