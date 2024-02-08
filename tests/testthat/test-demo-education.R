
#Ensures that variables are present and working on SCP
test_that("demo-education returns non-empty data frame", {
  education1 <- demo_education("wa", "county", "at least high school", "males")
  expect_true(!is.null(education1))
  expect_true(is.data.frame(education1))
  
  education2 <- demo_education("ca", "hsa", "less than 9th grade")
  expect_true(!is.null(education2))
  expect_true(is.data.frame(education2))
  
  education3 <- demo_education("usa", "state", "at least bachelors degree", 
                               "both sexes", "all races (includes hispanic)")
  expect_true(!is.null(education3))
  expect_true(is.data.frame(education3))
})


test_that("demo-education handles invalid education parameters", {
  expect_error(
    demo_education("wa", "county", "less than 9th grade", "males"),
    "For Less than 9th Grade, Race and Sex must be NULL"
  )
  expect_error(
    demo_education("wa", "county", "at least high school", "all races (includes hispanic", "females"),
    "For At Least High School, Race must be NULL and Sex must be NOT NULL"
  )
  expect_error(
    demo_education("wa", "county", "at least bachelors degree", "both sexes"),
    "For At Least Bachelors Degree, Race and Sex must be NOT NULL."
  )
})
