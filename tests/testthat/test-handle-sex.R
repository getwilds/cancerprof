#' Test Handle Sex
#' 
#' This testthat file tests the handle-sex function
test_that("handle sex correctly maps sex", {
  result <- sapply(c("both sexes", "males", "females"), handle_sex)
  expected <- c(`both sexes` = "0", males = "1", females = "2")
  
  expect_equal(result, expected)
})

test_that("handle sex expects errors for incorrect arguments", {
  expect_error(handle_sex("carrot"))
})