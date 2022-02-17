library(dplyr)
testthat::context("calc_jhu_global_cum")

test_that("Aggregation is correct",{
  expected <- readr::read_csv("test data/2021-05-25_time_series_covid19_deaths_global.csv") %>%
    dplyr::filter(`Country/Region` == "Australia") %>%
    dplyr::rename(location = `Country/Region`) %>%
    dplyr::select(-`Province/State`, -Long, -Lat) %>%
    tidyr::pivot_longer(
      matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
      names_to = "date",
      values_to = "cum") %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))) %>%
    dplyr::group_by(location, date) %>%
    dplyr::summarize(cum = sum(cum))
  
  
  actual <- covidData::load_jhu_data(
    temporal_resolution = 'daily',
    measure = 'deaths',
    as_of = '2021-05-25',
    geography = c("global")) %>%
    # filter to Australia
    dplyr::filter(location == "Australia") %>%
    dplyr::select(-inc)
  
  data <- merge(actual, expected, by = c("location", "date"))
  
  expect_true(all(data$cum.x - data$cum.y == 0))
})

test_that("Indexing is correct for european countries",{
  expected <- readr::read_csv("test data/2021-05-25_time_series_covid19_deaths_global.csv") %>%
    dplyr::filter(is.na(`Province/State`) & `Country/Region` == "France") %>%
    dplyr::select(-`Province/State`, -Long, -Lat)
  
  actual <- covidData::load_jhu_data(
    temporal_resolution = 'daily',
    measure = 'deaths',
    as_of = '2021-05-25',
    geography = c("global")) %>%
    # filter to France
    dplyr::filter(location == 'FR') %>%
    # format date
    dplyr::mutate(
      location = "France",
      date = gsub('(?<=\\/)0|^0', '', 
                              format(date, "%m/%d/%y"),
                              perl=TRUE)) %>%
    dplyr::select(-inc) %>%
    dplyr::rename(`Country/Region` = location) %>%
    tidyr::pivot_wider(names_from = date, values_from = cum)
  
  expect_identical(actual, expected)
})

