#' Test risk-colorectal-screening
#' 
#' This testthat file test the risk-colorectal-screening function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- risk_colorectal_screening("ever had fobt, ages 50-75", area="wa")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
screening_options <- list(
  screening1 = risk_colorectal_screening("ever had fobt, ages 50-75", area = "wa"),
  screening2 = risk_colorectal_screening("guidance sufficient crc, ages 50-75", area = "wa"),
  screening3 = risk_colorectal_screening("had colonoscopy in past 10 years, ages 50-75", area = "wa"),
  screening4 = risk_colorectal_screening("home blood stool test in the past year, ages 45-75",
                                         "all races (includes hispanic)", "both sexes"),
  screening5 = risk_colorectal_screening("received at least one recommended crc test, ages 45-75",
                                         "all races (includes hispanic)", "both sexes")
)

for (option_name in names(screening_options)) {
  test_that("risk-colorectal-screening returns non-empty data frame", {
    option <- screening_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

#risk-colorectal-screening must have 5 columns
test_that("risk-colorectal-screening has correct number of columns", {
  df1 <- risk_colorectal_screening("ever had fobt, ages 50-75", area="wa")
  df2 <- risk_colorectal_screening("home blood stool test in the past year, ages 45-75",
                                   "all races (includes hispanic)","both sexes")
  expected_columns1 <- 5
  expected_columns2 <- 6
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)
})

#test error handling
test_that("risk-colorectal-screening handles invalid colorectal_screening parameters", {
  expect_error(
    risk_colorectal_screening("ever had fobt, ages 50-75", race="all races (includes hispanic)"),
    "for this screening type, area must NOT be NULL and Race and Sex must be NULL"
  )
  expect_error(
    risk_colorectal_screening("home blood stool test in the past year, ages 45-75",
                              "all races (includes hispanic)"),
    "For this screening type, Race and Sex must not be NULL"
  )
})

#parameter
test_that("risk-colorectal-screening has correct parameters", {
  expect_error(risk_colorectal_screening())
})