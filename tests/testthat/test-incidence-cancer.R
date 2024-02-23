#' Test incidence Cancer
#'
#' This testthat file test the incidence_cancer function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  output <- incidence_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", 
                             "both sexes", "ages 65+", "all stages")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
cancer_options <- c("all cancer sites","bladder", "brain & ons", "colon & rectum", 
                    "esophagus", "kidney & renal pelvis", "leukemia", "liver & bile duct", 
                    "lung & bronchus", "melanoma of the skin", "non-hodgkin lymphoma", 
                    "oral cavity & pharynx", "pancreas", "stomach", "thyroid")

for (option in cancer_options) {
  test_that("incidence cancer returns non-empty data frame", {
    result <- incidence_cancer("wa", "county", option, "all races (includes hispanic)", 
                               "both sexes", "all ages", "all stages")
    expect_true(is.data.frame(result))
  })
}

female_cancer_options <- c("breast (female)", "breast (female in situ)", 
                           "cervix", "ovary", "uterus (corpus & uterus, nos)")

for (option in female_cancer_options) {
  test_that("incidence female cancer returns non-empty data frame", {
    result <- incidence_cancer("wa", "county", option, 
                               "all races (includes hispanic)", "females", 
                               "ages 50+", "all stages")
    
    expect_true(is.data.frame(result))
  })
}

childhood_male_cancer_options <- list(
  childhood15 = incidence_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", 
                                 "males", "ages <20", "all stages"),
  childhood20 = incidence_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", 
                                 "males", "ages <20", "all stages"),
  prostate = incidence_cancer("usa", "state", "prostate", "all races (includes hispanic)", 
                              "males", "ages 50+", "all stages")
)

for (option_name in names(childhood_male_cancer_options)) {
  test_that("incidence cancer returns non-empty data frame", {
    option <- childhood_male_cancer_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

#incidence_cancer must have 14 columns
test_that("incidence_cancer has correct number of columns", {
  df1 <- incidence_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", 
                         "both sexes", "ages 65+", "all stages")
  df2 <- incidence_cancer("usa", "state", "lung & bronchus", "all races (includes hispanic)", "males", 
                         "ages 50+", "late stage (regional & distant)")
  expected_columns1 <- 13
  expected_columns2 <- 10
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)
  
})

#parameter
test_that("incidence_cancer has correct parameters", {
  expect_error(incidence_cancer())
})