#' Test risk-diet-exercise
#' 
#' This testthat file test the risk-diet-exercise function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- risk_diet_exercise("bmi is healthy, ages 20+", 
                               "all races (includes hispanic)", "both sexes")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
diet_exercise_options <- c("bmi is healthy, ages 20+", "bmi is obese, ages 20+", 
                           "bmi is obese, high school survey", "bmi is overweight, high school survey",
                           "consumed 1 or more fruits per day", "consumed 1 or more vegetables per day",
                           "no leisure time physical activity")

for (option in diet_exercise_options) {
  test_that("risk_diet_exercise returns non-empty data frame", {
    result <- risk_diet_exercise(option, "all races (includes hispanic)", "both sexes")
    expect_true(is.data.frame(result))
  })
}

#risk-diet-exercise must have 5 columns
test_that("risk-diet-exercise has correct number of columns", {
  df1 <- risk_diet_exercise("bmi is healthy, ages 20+", 
                           "all races (includes hispanic)", "both sexes")
  df2 <- risk_diet_exercise("bmi is obese, high school survey", 
                            "all races (includes hispanic)", "males")
  expected_columns1 <- 6
  expected_columns2 <- 5
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)

})

#parameter
test_that("risk-diet-exercise has correct parameters", {
  expect_error(risk_diet_exercise())
})