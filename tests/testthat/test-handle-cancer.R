#' Test Handle cancer
#'
#' This testthat file tests the handle-cancer function
test_that("handle cancer correctly maps cancer", {
  result <- sapply(c("all cancer sites", "breast (female)", "prostate"), handle_cancer)
  expected <- c(`all cancer sites` = "001", `breast (female)` = "055", `prostate` = "066")

  expect_equal(result, expected)
})

test_that("handle cancer expects errors for incorrect arguments", {
  expect_error(handle_cancer("carrot"))
})
