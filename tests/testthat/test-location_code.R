library(dplyr)
library(covidData)
testthat::context("load_jhu")

test_that("location code filter works for US locations",{
  spatial_resolutions <- c("national", "state", "county")
  location_code <- c("01", "06059")
  actual <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                     location_code = location_code,
                                     as_of = "2022-02-01",
                                     temporal_resolution = "weekly",
                                     measure = "cases",
                                     geography = c("US"))
  
  expected <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                       location_code = NULL,
                                       as_of = "2022-02-01",
                                       temporal_resolution = "weekly",
                                       measure = "cases",
                                       geography = c("US")) %>%
    dplyr::filter(location %in% location_code)
  
  expect_true(all(unique(actual$location) ==  location_code))
  expect_identical(actual, expected)
})

test_that("location code filter works for global locations",{
  location_code <- c("FR", "GB")
  actual <- covidData::load_jhu_data(spatial_resolution = NULL,
                                     location_code = location_code,
                                     as_of = "2022-02-01",
                                     temporal_resolution = "weekly",
                                     measure = "cases",
                                     geography = c("global"))
  
  expected <- covidData::load_jhu_data(spatial_resolution = NULL,
                                       location_code = NULL,
                                       as_of = "2022-02-01",
                                       temporal_resolution = "weekly",
                                       measure = "cases",
                                       geography = c("global")) %>%
    dplyr::filter(location %in% location_code)
  
  expect_true(all(unique(actual$location) == location_code))
  expect_identical(actual, expected)
})