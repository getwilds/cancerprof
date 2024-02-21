#' Test Handle vaccine
#' 
#' This testthat file tests the handle-vaccine function
test_that("handle vaccine correctly maps vaccine", {
  result <- sapply(c("percent with up to date hpv vaccination coverage, ages 13-15",
                     "percent with up to date hpv vaccination coverage, ages 13-17"), 
                   handle_vaccine)
  expected <- c(`percent with up to date hpv vaccination coverage, ages 13-15` = "v281",
                `percent with up to date hpv vaccination coverage, ages 13-17` = "v282")
  
  expect_equal(result, expected)
})

test_that("handle vaccine expects errors for incorrect arguments", {
  expect_error(handle_vaccine("carrot"))
})