library(readr)
library(purrr)
library(tibble)
library(here)

# after this the working directory should be covidData
setwd(here())

# read in fips codes data -- need to do this instead of referencing
# covidData::fips_codes because the covidData package may not be installed yet.
load("data/fips_codes.rdata")

# load preprocessing functions in R/healthdata_data.R
# they are defined there so they can be unit tested, but loaded manually here
# so that data can be processed if the covidData package is not installed yet.
source("R/healthdata_data.R")

# read in csv files from the data-raw directory
timeseries_files <- Sys.glob("data-raw/healthdata/timeseries/*.csv")
daily_files <- Sys.glob("data-raw/healthdata/daily/*.csv")

message("time series files")
message(timeseries_files)
message("daily files")
message(daily_files)

healthdata_hosp_data_ts <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(timeseries_files, "/"),
    function(x) substr(x[4], 1, 10)),
  data = purrr::map(
    timeseries_files,
    function(filename) suppressMessages(readr::read_csv(filename)))
)

healthdata_hosp_data_daily <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(daily_files, "/"),
    function(x) substr(x[4], 1, 10)),
  date = purrr::map_chr(
    strsplit(daily_files, "/"),
    function(x) substr(x[4], 12, 21)),
  data = purrr::map(
    daily_files,
    function(filename) suppressMessages(readr::read_csv(filename)))
)

# Get all unique issue dates across all data files
all_issue_dates <- c(
  healthdata_hosp_data_ts$issue_date,
  healthdata_hosp_data_daily$issue_date
) %>%
  unique() %>%
  sort()

healthdata_hosp_data <- purrr::map_dfr(
  all_issue_dates,
  function(issue_date) {
    reconstructed_data_one_issue <- build_healthdata_data(
      issue_date,
      healthdata_hosp_data_ts = healthdata_hosp_data_ts,
      healthdata_hosp_data_daily = healthdata_hosp_data_daily
    )

    preprocess_healthdata_data(reconstructed_data_one_issue, fips_codes)
  }
)

save(healthdata_hosp_data,
  file = "data/healthdata_hosp_data.rdata")
message("data files")
message(list.files("data"))
