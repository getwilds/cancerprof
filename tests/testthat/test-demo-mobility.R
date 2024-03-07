#' Test demo-mobility
#' 
#' This testthat file test the demo-mobility function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_mobility("wa", "county", "moved, different county, same state (in past year)")
  
  expect_true(inherits(output, "data.frame"))
})

mobility_options <- c("i haven't moved (in past year)", 
                      "moved from outside us (in past year)", 
                      "moved, different state (in past year)", 
                      "moved, different county, same state (in past year)", 
                      "moved, same county (in past year)") 


for (option in mobility_options) {
  test_that("demo-mobility returns non-empty data frame", {
    skip_on_cran()
    expect_true(is.data.frame(demo_mobility("wa", "county", option)))
  })
}

#demo-mobility must have 5 columns
test_that("demo-mobility has correct number of columns", {
  skip_on_cran()
  df <- demo_mobility("wa", "county", "moved, different county, same state (in past year)")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("demo-mobility has correct parameters", {
  expect_error(demo_mobility())
})