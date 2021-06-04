library(dplyr)
library(lubridate)
testthat::context("load_jhu_data-reconstruct_time_series")


test_that("reconstruct time series data for cumulative deaths count",{
  expected <- readr::read_csv("test data/2021-01-11_time_series_covid19_deaths_US.csv") %>%
    # filter fips 
    dplyr::filter(FIPS > 100 & FIPS < 80001)
  
  data <- covidData::load_jhu_data(
    spatial_resolution = 'county',
    temporal_resolution = 'daily',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-01-11') %>%
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
  expected <- readr::read_csv("test data/2021-01-11_time_series_covid19_confirmed_US.csv") %>%
    # filter fips 
    dplyr::filter(FIPS > 100 & FIPS < 80001)
  
  data <- covidData::load_jhu_data(
    spatial_resolution = 'county',
    temporal_resolution = 'daily',
    measure = 'cases',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-01-11') %>%
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

test_that("reconstruct time series data for cumulative cases count in global locations",{
  expected <- readr::read_csv("test data/2021-05-25_time_series_covid19_confirmed_global.csv") %>%
    calc_jhu_global_cum() %>%
    dplyr::filter(!`Country/Region` %in% c("Diamond Princess", "MS Zaandam"))
  
  data <- covidData::load_jhu_data(
    temporal_resolution = 'daily',
    measure = 'cases',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-05-25',
    geography = c("global")) %>%
    dplyr::left_join(y = covidData::global_locations, 
                     by = c("location" = "location")) %>%
    dplyr::select(-location) %>%
    dplyr::rename(location = location_name)
  
  actual <- data %>%
    # format date
    dplyr::mutate(date = gsub('(?<=\\/)0|^0', '', 
                              format(date, "%m/%d/%y"),
                              perl=TRUE)) %>%
    dplyr::select(-inc) %>%
    dplyr::rename(`Country/Region` = location) %>%
    tidyr::pivot_wider(names_from = date, values_from = cum)
  
  expect_identical(actual, expected)
  
})

test_that("reconstruct time series data for cumulative death count in global locations",{
  expected <- readr::read_csv("test data/2021-05-25_time_series_covid19_deaths_global.csv") %>%
    calc_jhu_global_cum() %>%
    dplyr::filter(!`Country/Region` %in% c("Diamond Princess", "MS Zaandam"))
  
  data <- covidData::load_jhu_data(
    temporal_resolution = 'daily',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-05-25',
    geography = c("global")) %>%
    dplyr::left_join(y = covidData::global_locations, 
                     by = c("location" = "location")) %>%
    dplyr::select(-location) %>%
    dplyr::rename(location = location_name)
  
  actual <- data %>%
    # format date
    dplyr::mutate(date = gsub('(?<=\\/)0|^0', '', 
                              format(date, "%m/%d/%y"),
                              perl=TRUE)) %>%
    dplyr::select(-inc) %>%
    dplyr::rename(`Country/Region` = location) %>%
    tidyr::pivot_wider(names_from = date, values_from = cum)
  
  expect_identical(actual, expected)
  
})

