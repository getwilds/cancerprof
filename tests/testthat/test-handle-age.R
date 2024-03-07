#' Test Handle age
#'
#' This testthat file tests the handle-age function
test_that("handle age correctly maps age", {
  result <- sapply(
    c(
      "under 19 years",
      "under 65 years",
      "all ages"
    ),
    handle_age
  )
  expected <- c(
    `under 19 years` = "175",
    `under 65 years` = "006",
    `all ages` = "001"
  )

  expect_equal(result, expected)
})

test_that("handle age expects errors for incorrect arguments", {
  expect_error(handle_age("carrot"))
})
