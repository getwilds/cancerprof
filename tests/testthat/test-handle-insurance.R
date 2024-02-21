#' Test Handle insurance
#' 
#' This testthat file tests the handle-insurance function
test_that("handle insurance correctly maps insurance", {
  result <- sapply(c("% insured in demographic group, all income levels", 
                     "% uninsured in demographic group, all income levels",
                     "% insured in demographic group, people at or below 138% of poverty"), 
                   handle_insurance)
  expected <- c(`% insured in demographic group, all income levels` = "00030",
                `% uninsured in demographic group, all income levels` = "00040",
                `% insured in demographic group, people at or below 138% of poverty` = "00033")
  
  expect_equal(result, expected)
})

test_that("handle insurance expects errors for incorrect arguments", {
  expect_error(handle_insurance("carrot"))
})