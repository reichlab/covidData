library(dplyr)
testthat::context("as_of")

test_that("load_jhu_data as_of: as_of existing issue_date", {
  issue_date_data <- covidData::load_jhu_data(
    issue_date = "2020-11-02",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "deaths",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  as_of_data <- covidData::load_jhu_data(
    as_of = "2020-11-02",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "deaths",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  expect_identical(issue_date_data, as_of_data)
})

test_that("load_jhu_data as_of: as_of non-existant issue_date", {
  issue_date_data <- covidData::load_jhu_data(
    issue_date = "2020-03-31",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "deaths",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  as_of_data <- covidData::load_jhu_data(
    as_of = "2020-04-01",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "deaths",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  expect_identical(issue_date_data, as_of_data)
})
test_that("load_healthdata_data as_of: as_of existing issue_date", {
  issue_date_data <- covidData::load_healthdata_data(
    issue_date = "2020-12-02",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "hospitalizations",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  as_of_data <- covidData::load_healthdata_data(
    as_of = "2020-12-02",
    spatial_resolution = "state",
    temporal_resolution = "daily",
    measure = "hospitalizations",
    replace_negatives = FALSE,
    adjustment_cases = "none")

  expect_identical(issue_date_data, as_of_data)
})

# Currently load_healthdata_data contains all relevant issue dates,
# potentially with missing data filled in as necessary.

# test_that("load_healthdata_data as_of: as_of non-existant issue_date", {
#   issue_date_data <- covidData::load_healthdata_data(
#     issue_date = "2020-03-31",
#     spatial_resolution = "state",
#     temporal_resolution = "daily",
#     measure = "deaths",
#     replace_negatives = FALSE,
#     adjustment_cases = "none")

#   as_of_data <- covidData::load_healthdata_data(
#     as_of = "2020-04-01",
#     spatial_resolution = "state",
#     temporal_resolution = "daily",
#     measure = "deaths",
#     replace_negatives = FALSE,
#     adjustment_cases = "none")

#   expect_identical(issue_date_data, as_of_data)
# })
