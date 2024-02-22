#' Test Mortality Cancer
#'
#' This testthat file test the mortality_cancer function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  output <- mortality_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", 
                             "both sexes", "ages 65+")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
cancer_options <- c("all cancer sites","bladder", "brain & ons", "colon & rectum", 
                    "esophagus", "kidney & renal pelvis", "leukemia", "liver & bile duct", 
                    "lung & bronchus", "melanoma of the skin", "non-hodgkin lymphoma", 
                    "oral cavity & pharynx", "pancreas", "stomach", "thyroid")

for (option in cancer_options) {
  test_that("mortality cancer returns non-empty data frame", {
    result <- mortality_cancer("wa", "county", option, "all races (includes hispanic)", 
                               "both sexes", "all ages")
    expect_true(is.data.frame(result))
  })
}

female_cancer_options <- c("breast (female)", "cervix", "ovary", "uterus (corpus & uterus, nos)")

for (option in female_cancer_options) {
  test_that("mortality female cancer returns non-empty data frame", {
    result <- mortality_cancer("wa", "county", option, "all races (includes hispanic)", "females", "ages 50+")
    
    expect_true(is.data.frame(result))
  })
}

childhood_male_cancer_options <- list(
  childhood15 = mortality_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", 
                                 "males", "ages <20"),
  childhood20 = mortality_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", 
                                 "males", "ages <20"),
  prostate = mortality_cancer("usa", "state", "prostate", "all races (includes hispanic)", 
                              "males", "ages 50+")
)

for (option_name in names(childhood_male_cancer_options)) {
  test_that("mortality cancer returns non-empty data frame", {
    option <- childhood_male_cancer_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

#mortality_cancer must have 14 columns
test_that("mortality_cancer has correct number of columns", {
  df <- mortality_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", 
                         "both sexes", "ages 65+")
  expected_columns <- 14
  expect_equal(ncol(df), expected_columns)
  
})

#test error handling
test_that("mortality_cancer handles invalid cancer parameters", {
  expect_error(
    mortality_cancer(area="wa", areatype="county", cancer="ovary", 
                     race="all races (includes hispanic)", 
                     sex="both sexes", age="ages 50+"),
    "For this cancer type, sex must be females"
  )
  expect_error(
    mortality_cancer("usa", "state", "prostate", "all races (includes hispanic)", 
                     "both sexes", "ages 50+"),
    "For prostate cancer, sex must be males."
  )
})

#parameter
test_that("mortality_cancer has correct parameters", {
  expect_error(mortality_cancer())
})

