library(dplyr)
testthat::context("load_healthdata_data")

test_that("load_healthdata_data expected locations: state only", {
  expected_locations <- covidData::fips_codes %>%
    dplyr::filter(
      nchar(location) == 2,
      !(abbreviation %in% c("AS", "GU", "MP", "UM", "US"))
    ) %>%
    dplyr::pull(location) %>%
    sort()

  actual_locations <- covidData::load_healthdata_data(
    as_of = "2020-12-02",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "hosp",
    replace_negatives = FALSE,
    adjustment_cases = "none") %>%
    dplyr::pull(location) %>%
    unique() %>%
    sort()

  expect_identical(expected_locations, actual_locations)
})

test_that("load_healthdata_data expected locations: national only", {
  expected_locations <- "US"

  actual_locations <- covidData::load_healthdata_data(
    as_of = "2020-12-02",
    spatial_resolution = "national",
    temporal_resolution = "daily",
    measure = "hosp",
    replace_negatives = FALSE,
    adjustment_cases = "none") %>%
    dplyr::pull(location) %>%
    unique() %>%
    sort()

  expect_identical(expected_locations, actual_locations)
})

test_that("load_healthdata_data expected locations: state and national", {
  expected_locations <- covidData::fips_codes %>%
    dplyr::filter(
      nchar(location) == 2,
      !(abbreviation %in% c("AS", "GU", "MP", "UM"))
    ) %>%
    dplyr::pull(location) %>%
    sort()

  actual_locations <- covidData::load_healthdata_data(
    as_of = "2020-12-02",
    spatial_resolution = c("state", "national"),
    temporal_resolution = "daily",
    measure = "hosp",
    replace_negatives = FALSE,
    adjustment_cases = "none") %>%
    dplyr::pull(location) %>%
    unique() %>%
    sort()

  expect_identical(expected_locations, actual_locations)
})



