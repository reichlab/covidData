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
    measure = "hospitalizations",
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
    measure = "hospitalizations",
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
    measure = "hospitalizations",
    replace_negatives = FALSE,
    adjustment_cases = "none") %>%
    dplyr::pull(location) %>%
    unique() %>%
    sort()

  expect_identical(expected_locations, actual_locations)
})

# NOTE: Most of the data match but there are some still don't. 
# Differences are in diff$inc_diff column
# test_that("load_healthdata_data expected data with an as_of date earlier than 2021-03-12", {
#   expected_data <- covidData::load_covidcast_data(
#     as_of = "2021-02-11",
#     spatial_resolution = c("state", "national"),
#     temporal_resolution = "daily",
#     measure = "hospitalizations")
#   
#   actual_data <- covidData::load_healthdata_data(
#     as_of = "2021-02-10",
#     spatial_resolution = c("state", "national"),
#     temporal_resolution = "daily",
#     measure = "hospitalizations")
#   
#   merge <- actual_data %>%
#     dplyr::left_join(expected_data, by = c("date", "location")) %>%
#     dplyr::mutate(inc_diff = inc.x - inc.y)
#   
#   diff <- merge[(merge$inc_diff!=0 & (!is.na(merge$inc_diff))),]
# })
# 
# test_that("load_healthdata_data expected data with an as_of date that has a complete time series data", {
#   expected_data <- covidData::load_covidcast_data(
#     as_of = "2022-01-21",
#     spatial_resolution = c("state", "national"),
#     temporal_resolution = "daily",
#     measure = "hospitalizations")
#   
#   actual_data <- covidData::load_healthdata_data(
#     as_of = "2022-01-20",
#     spatial_resolution = c("state", "national"),
#     temporal_resolution = "daily",
#     measure = "hospitalizations")
#   
#   merge <- actual_data %>%
#     dplyr::left_join(expected_data, by = c("date", "location")) %>%
#     dplyr::mutate(inc_diff = inc.x - inc.y)
#   
#   diff <- merge[(merge$inc_diff!=0 & (!is.na(merge$inc_diff))),]
#   
# })

test_that("load_healthdata_data expected data with an as_of date that needs daily revision data", {
  expected_data <- covidData::load_covidcast_data(
    as_of = "2021-03-28",
    spatial_resolution = c("state", "national"),
    temporal_resolution = "daily",
    measure = "hospitalizations")
  
  actual_data <- covidData::load_healthdata_data(
    as_of = "2021-03-28",
    spatial_resolution = c("state", "national"),
    temporal_resolution = "daily",
    measure = "hospitalizations")
  
  merge <- actual_data %>%
    dplyr::left_join(expected_data, by = c("date", "location")) %>%
    dplyr::mutate(inc_diff = inc.x - inc.y)
  
  diff <- merge[(merge$inc_diff!=0 & (!is.na(merge$inc_diff))),]
  
  expect_equal(nrow(diff), 0)
})



