#' Test Handle Non-english
#' 
#' This testthat file tests the handle_non_english function
test_that("handle_non_english correctly maps language", {
  result <- sapply(c("language isolation"), handle_non_english)
  expected <- c(`language isolation` = "00015")
  
  expect_equal(result, expected)
})

test_that("handle_non_english expects errors for incorrect arguments", {
  expect_error(handle_non_english("carrot"))
})