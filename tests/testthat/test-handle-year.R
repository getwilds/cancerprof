#' Test Handle year
#'
#' This testthat file tests the handle-year function
test_that("handle year correctly maps year", {
  result <- sapply(
    c(
      "latest 5 year average",
      "latest single year (us by state)"
    ),
    handle_year
  )
  expected <- c(
    `latest 5 year average` = "0",
    `latest single year (us by state)` = "1"
  )

  expect_equal(result, expected)
})

test_that("handle year expects errors for incorrect arguments", {
  expect_error(handle_year("carrot"))
})
