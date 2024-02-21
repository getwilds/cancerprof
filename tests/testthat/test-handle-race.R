#' Test Handle race
#' 
#' This testthat file tests the handle-race function
test_that("handle race correctly maps race", {
  result <- sapply(c("all races (includes hispanic)", "white (non-hispanic)", 
                     "asian non-hispanic"), handle_race)
  expected <- c(`all races (includes hispanic)` = "00", 
                `white (non-hispanic)` = "07", 
                `asian non-hispanic` = "49")
  
  expect_equal(result, expected)
})

test_that("handle race expects errors for incorrect arguments", {
  expect_error(handle_race("carrot"))
})