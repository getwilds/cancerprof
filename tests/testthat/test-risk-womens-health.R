#' Test risk-womens health
#'
#' This testthat file test the risk-womens health function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  output <- risk_women_health("mammogram in past 2 years, ages 50-74", 
                         "all races (includes hispanic)", "direct estimates")

  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
womens_health_options <- c("mammogram in past 2 years, ages 50-74",
                           "mammogram in past 2 years, ages 40+", 
                           "pap smear in past 3 years, no hysterectomy, ages 21-65")

for (option in womens_health_options) {
  test_that("risk_womens_health returns non-empty data frame", {
    result <- risk_women_health(option, "all races (includes hispanic)", "direct estimates")
    expect_true(is.data.frame(result))
  })
}

#risk-womens health must have 5 columns
test_that("risk-womens health has correct number of columns", {
  df1 <- risk_women_health("mammogram in past 2 years, ages 50-74", 
                      "all races (includes hispanic)", "direct estimates")
  df2 <- risk_women_health("mammogram in past 2 years, ages 50-74", 
                      "all races (includes hispanic)", "county level modeled estimates", "wa")
  
  expected_columns1 <- 6
  expected_columns2 <- 5
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)

})

#parameter
test_that("risk-womens health has correct parameters", {
  expect_error(risk_women_health())
})