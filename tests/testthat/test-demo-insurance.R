#' Test demo-insurance
#' 
#' This testthat file test the demo-insurance function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- demo_insurance("usa", "state", "% Insured in demographic group, all income levels", 
                           "both sexes", "under 19 years", "all races (includes hispanic)")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
insurance_options <- c("% Insured in demographic group, all income levels", 
                       "% Insured in demographic group, people at or below 138% of Poverty",
                       "% Insured in demographic group, people at or below 200% of Poverty", 
                       "% Insured in demographic group, people at or below 250% of Poverty", 
                       "% Insured in demographic group, people at or below 400% of Poverty",
                       "% Insured in demographic group, people between 138% - 400% of poverty", 
                       "% uninsured in demographic group, all income levels",
                       "% uninsured in demographic group, people at or below 138% of Poverty", 
                       "% uninsured in demographic group, people at or below 200% of Poverty",
                       "% uninsured in demographic group, people at or below 250% of Poverty", 
                       "% uninsured in demographic group, people at or below 400% of Poverty",
                       "% uninsured in demographic group, people between 138% - 400% of poverty")

for (option in insurance_options) {
  test_that("demo-insurance returns non-empty data frame", {
    expect_true(is.data.frame(demo_insurance("usa", "state", option, "both sexes", 
                                             "under 19 years", "all races (includes hispanic)")))
  })
}

#demo-insurance must have 5 columns
test_that("demo-insurance has correct number of columns", {
  df <- demo_insurance("wa", "hsa", "% Insured in demographic group, all income levels", 
                       "males", "18 to 64 years")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#test error handling

#write one for error handling in insurance - mention in the message how values could have chnaged in SCP
test_that("demo-insurance handles invalid insurance parameters", {
  expect_error(
    demo_insurance("wa", "county", "% Insured in demographic group, all income levels", 
                   "males", "18 to 64 years", "all races (includes hispanic)")
  )
  expect_error(
    demo_insurance("usa", "state", "% Insured in demographic group, all income levels", 
                   "males", "18 to 64 years")
  )
})

#parameter
test_that("demo-insurance has correct parameters", {
  expect_error(demo_insurance())
})