#' Test Handle svi
#' 
#' This testthat file tests the handle-svi function
test_that("handle svi correctly maps svi", {
  result <- sapply(c("overall", "socioeconomic status", 
                     "household characteristics"), handle_svi)
  expected <- c(`overall` = "03010", `socioeconomic status` = "03011", 
                `household characteristics` = "03012")
  
  expect_equal(result, expected)
})

test_that("handle svi expects errors for incorrect arguments", {
  expect_error(handle_svi("carrot"))
})