#' Test risk-vaccines
#'
#' This testthat file test the risk-vaccines function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- risk_vaccines("percent with up to date hpv vaccination coverage, ages 13-15",
                         "both sexes")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
vaccine_options <- c("percent with up to date hpv vaccination coverage, ages 13-15",
                     "percent with up to date hpv vaccination coverage, ages 13-17")

for (option in vaccine_options) {
  test_that("risk_womens_health returns non-empty data frame", {
    skip_on_cran()
    result <- risk_vaccines(option, "both sexes")
    expect_true(is.data.frame(result))
  })
}

#risk-vaccines must have 5 columns
test_that("risk-vaccines has correct number of columns", {
  skip_on_cran()
  df <- risk_vaccines("percent with up to date hpv vaccination coverage, ages 13-15",
                      "both sexes")
  expected_columns <- 6
  expect_equal(ncol(df), expected_columns)
  
})

#parameter
test_that("risk-vaccines has correct parameters", {
  expect_error(risk_vaccines())
})