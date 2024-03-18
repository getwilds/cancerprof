#' Test Handle alcohol
#'
#' This testthat file tests the handle-age function
test_that("handle_alcohol correctly maps alcohol", {
  result <- sapply(c("binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+"), handle_alcohol)
  expected <- c(`binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+` = "v505")

  expect_equal(result, expected)
})

test_that("handle_alcohol expects errors for incorrect arguments", {
  expect_error(handle_alcohol("carrot"))
})
