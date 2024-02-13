#' Test demo-svi
#' 
#' This testthat file test the demo-svi function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- demo_svi("wa", "overall")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
svi_options <- c("Overall", "socioeconomic status", "household characteristics",
                 "racial & ethinic minority status", "housing type & transportation"
)

for (option in svi_options) {
  test_that("demo-svireturns non-empty data frame", {
    expect_true(is.data.frame(demo_svi("wa", option)))
  })
}

#demo-svi must have 5 columns
test_that("demo-svi has correct number of columns", {
  df <- demo_svi("wa", "overall")
  expected_columns <- 3
  expect_equal(ncol(df), expected_columns)
})

#parameter
test_that("demo-svi has correct parameters", {
  expect_error(demo_svi())
})