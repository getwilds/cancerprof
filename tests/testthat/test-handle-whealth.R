#' Test Handle Women's Health
#' 
#' This testthat file tests the handle-whealth function
test_that("handle whealth correctly maps whealth", {
  result <- sapply(c("mammogram in past 2 years, ages 50-74", 
                     "mammogram in past 2 years, ages 40+", 
                     "pap smear in past 3 years, no hysterectomy, ages 21-65"), 
                   handle_whealth)
  expected <- c(`mammogram in past 2 years, ages 50-74` = "v05", 
                `mammogram in past 2 years, ages 40+` = "v06", 
                `pap smear in past 3 years, no hysterectomy, ages 21-65` = "v17")
  
  expect_equal(result, expected)
})

test_that("handle whealth expects errors for incorrect arguments", {
  expect_error(handle_whealth("carrot"))
})