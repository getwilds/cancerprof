#' Test Handle smoking
#'
#' This testthat file tests the handle-smoking function
test_that("handle smoking correctly maps age", {
  result <- sapply(c(
    "smokers (current); ages 18+",
    "smokers (ever); ages 18+",
    "smoking laws (any)"
  ), handle_smoking)
  expected <- c(
    `smokers (current); ages 18+` = "v19",
    `smokers (ever); ages 18+` = "v28",
    `smoking laws (any)` = "v44"
  )

  expect_equal(result, expected)
})

test_that("handle smoking expects errors for incorrect arguments", {
  expect_error(handle_smoking("carrot"))
})
