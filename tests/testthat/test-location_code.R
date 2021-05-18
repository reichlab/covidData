library(dplyr)
library(covidData)
testthat::context("load_jhu")

test_that("location code filter works for US hub",{
  spatial_resolutions <- c("national", "state", "county")
  location_code <- c("01", "06059")
  actual <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                     location_code = location_code,
                                     temporal_resolution = "weekly",
                                     measure = "cases",
                                     hub = c("US"))
  
  expected <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                       location_code = NULL,
                                       temporal_resolution = "weekly",
                                       measure = "cases",
                                       hub = c("US")) %>%
    dplyr::filter(location %in% location_code)
  
  expect_true(all(unique(actual$location) ==  location_code))
  expect_identical(actual, expected)
})

test_that("location code filter works for ECDC hub",{
  location_code <- c("FR", "GB")
  actual <- covidData::load_jhu_data(spatial_resolution = NULL,
                                     location_code = location_code,
                                     temporal_resolution = "weekly",
                                     measure = "cases",
                                     hub = c("ECDC"))
  
  expected <- covidData::load_jhu_data(spatial_resolution = NULL,
                                       location_code = NULL,
                                       temporal_resolution = "weekly",
                                       measure = "cases",
                                       hub = c("ECDC")) %>%
    dplyr::filter(location %in% c("France", "United Kingdom"))
  
  expect_true(all(unique(actual$location) == c("France", "United Kingdom")))
  expect_identical(actual, expected)
})