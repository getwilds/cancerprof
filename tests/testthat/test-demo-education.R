test_that("demo-education", {
  test_education1 <- demo_education("wa", "county", "at least high school", "males")
  
  expect_type(test_education1, "list")
  
  
  expect_error(
    demo_education("wa", "county", "less than 9th grade", "males"),
    "For Less than 9th Grade, Race and Sex must be NULL"
  )
  expect_error(
    demo_education("wa", "county", "at least high school", "all races (includes hispanic", "females"),
    "For At Least High School, Race must be NULL and Sex must be NOT NULL"
  )
})
