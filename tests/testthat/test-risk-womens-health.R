#' Test risk-womens health
#'
#' This testthat file test the risk-womens health function
#'
#tests class and typeof output
# test_that("Output data type is correct", {
#   output <- risk_womens_health("bmi is healthy, ages 20+",
#                                "all races (includes hispanic)", "both sexes")
# 
#   expect_true(inherits(output, "data.frame"))
# })
# 
# #Ensures that variables are present and working on SCP
# womens_health_options <- c("bmi is healthy, ages 20+", "bmi is obese, ages 20+",
#                            "bmi is obese, high school survey", "bmi is overweight, high school survey",
#                            "consumed 1 or more fruits per day", "consumed 1 or more vegetables per day",
#                            "no leisure time physical activity")
# 
# for (option in womens_health_options) {
#   test_that("risk_womens_health returns non-empty data frame", {
#     result <- risk_womens_health(option, "all races (includes hispanic)", "both sexes")
#     expect_true(is.data.frame(result))
#   })
# }
# 
# #risk-womens health must have 5 columns
# test_that("risk-womens health has correct number of columns", {
#   df1 <- risk_womens_health("bmi is healthy, ages 20+",
#                             "all races (includes hispanic)", "both sexes")
#   df2 <- risk_womens_health("bmi is obese, high school survey",
#                             "all races (includes hispanic)", "males")
#   expected_columns1 <- 6
#   expected_columns2 <- 5
#   expect_equal(ncol(df1), expected_columns1)
#   expect_equal(ncol(df2), expected_columns2)
# 
# })
# 
# #parameter
# test_that("risk-womens health has correct parameters", {
#   expect_error(risk_womens_health())
# })