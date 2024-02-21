#' Test Handle mobility
#' 
#' This testthat file tests the handle-mobility function
test_that("handle mobility correctly maps mobility", {
  result <- sapply(c("i haven't moved (in past year)",
                     "moved from outside us (in past year)", 
                     "moved, same county (in past year)"), handle_mobility)
  expected <- c(`i haven't moved (in past year)` = "00017", 
                `moved from outside us (in past year)` = "00021", 
                `moved, same county (in past year)` = "00018")
  
  expect_equal(result, expected)
})

test_that("handle mobility expects errors for incorrect arguments", {
  expect_error(handle_mobility("carrot"))
})