#' Test Handle datatype
#' 
#' This testthat file tests the handle-datatype function
test_that("handle datatype correctly maps datatype", {
  result <- sapply(c("direct estimates", "county level modeled estimates"), handle_datatype)
  expected <- c(`direct estimates` = "0", `county level modeled estimates` = "1")
  
  expect_equal(result, expected)
})

test_that("handle datatype expects errors for incorrect arguments", {
  expect_error(handle_datatype("carrot"))
})