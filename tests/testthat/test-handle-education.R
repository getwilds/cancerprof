#' Test Handle education
#' 
#' This testthat file tests the handle-education function
test_that("handle education correctly maps education", {
  result <- sapply(c("less than 9th grade", "at least high school", 
                     "at least bachelors degree"), handle_education)
  expected <- c(`less than 9th grade` = "00004", `at least high school` = "00109", 
                `at least bachelors degree` = "00006")
  
  expect_equal(result, expected)
})

test_that("handle education expects errors for incorrect arguments", {
  expect_error(handle_education("carrot"))
})