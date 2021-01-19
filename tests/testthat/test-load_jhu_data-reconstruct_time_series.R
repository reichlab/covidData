library(dplyr)
library(lubridate)
testthat::context("load_jhu_data-reconstruct_time_series")


test_that("reconstruct time series data for cumulative deaths count",{
  expected <- readr::read_csv("test data/2021-01-13_time_series_covid19_deaths_US.csv") %>%
    # filter fips 
    dplyr::filter(FIPS > 100 & FIPS < 80001)
  
  data <- covidData::load_jhu_data(
    spatial_resolution = 'county',
    temporal_resolution = 'daily',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-01-13') %>%
    # take out leading 0 in location 
    dplyr::mutate(location = sub("^0+", "", location))
  
  actual <- data %>%
    # format date
    dplyr::mutate(date = gsub('(?<=\\/)0|^0', '', 
                              format(date, "%m/%d/%y"),
                              perl=TRUE),
                  location = as.numeric(location)) %>%
    dplyr::select(-inc) %>%
    dplyr::rename(FIPS = location) %>%
    tidyr::pivot_wider(names_from = date, values_from = cum)
  
  expected <- expected %>%
    dplyr::select(-UID, -iso2, -iso3, -code3, -Admin2, 
                  -Province_State, -Country_Region, -Lat,
                  -Long_, -Combined_Key, -Population)
    
  expect_identical(actual, expected)
  
})


test_that("reconstruct time series data for cumulative cases count",{
  expected <- readr::read_csv("test data/2021-01-13_time_series_covid19_confirmed_US.csv") %>%
    # filter fips 
    dplyr::filter(FIPS > 100 & FIPS < 80001)
  
  data <- covidData::load_jhu_data(
    spatial_resolution = 'county',
    temporal_resolution = 'daily',
    measure = 'cases',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-01-13') %>%
    # take out leading 0 in location 
    dplyr::mutate(location = sub("^0+", "", location))
  
  actual <- data %>%
    # format date
    dplyr::mutate(date = gsub('(?<=\\/)0|^0', '', 
                              format(date, "%m/%d/%y"),
                              perl=TRUE),
                  location = as.numeric(location)) %>%
    dplyr::select(-inc) %>%
    dplyr::rename(FIPS = location) %>%
    tidyr::pivot_wider(names_from = date, values_from = cum)
  
  expected <- expected %>%
    dplyr::select(-UID, -iso2, -iso3, -code3, -Admin2, 
                  -Province_State, -Country_Region, -Lat,
                  -Long_, -Combined_Key)
  
  expect_identical(actual, expected)
  
})

