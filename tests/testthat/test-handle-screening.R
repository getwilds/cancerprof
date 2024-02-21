#' Test Handle screening
#' 
#' This testthat file tests the handle-screening function
test_that("handle screening correctly maps screening", {
  result <- sapply(c("ever had fobt, ages 50-75", 
                     "guidance sufficient crc, ages 50-75", 
                     "home blood stool test in the past year, ages 45-75"), handle_screening)
  expected <- c(`ever had fobt, ages 50-75` = "v304", 
                `guidance sufficient crc, ages 50-75` = "v303", 
                `home blood stool test in the past year, ages 45-75` = "v520")
  
  expect_equal(result, expected)
})

test_that("handle screening expects errors for incorrect arguments", {
  expect_error(handle_screening("carrot"))
})