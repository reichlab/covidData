library(readr)
library(purrr)
library(tibble)
library(here)

# after this the working directory should be covidData
setwd(here())

#' pull data from a timeseries update or build data intermediate between time
#' series updates by augmenting with daily values
build_data <- function(
  issue_date,
  healthdata_hosp_data_ts,
  healthdata_hosp_data_daily
  ) {
  if (issue_date %in% healthdata_hosp_data_ts$issue_date) {
    result <- healthdata_hosp_data_ts %>%
      dplyr::filter(issue_date == UQ(issue_date))
  } else {
    last_weekly <- healthdata_hosp_data_ts %>%
      dplyr::filter(issue_date < UQ(issue_date)) %>%
      dplyr::slice_max(issue_date)
    last_issue_date <- lubridate::ymd(last_weekly$issue_date)
    num_dates_to_add <- lubridate::ymd(issue_date) - last_issue_date

    for (i in seq_len(num_dates_to_add)) {
      new_date <- last_issue_date + i
      if (as.character(new_date) %in% healthdata_hosp_data_daily$issue_date) {
        last_weekly$data[[1]] <- dplyr::bind_rows(
          last_weekly$data[[1]],
          healthdata_hosp_data_daily$data[[which(
            healthdata_hosp_data_daily$issue_date == as.character(new_date)
          )]] %>%
          dplyr::mutate(date = new_date)
        )
      } else {
        required_locations <- unique(last_weekly$data[[1]]$state)
        last_weekly$data[[1]] <- dplyr::bind_rows(
          last_weekly$data[[1]],
          tidyr::expand_grid(
            state = required_locations,
            date = new_date,
            previous_day_admission_adult_covid_confirmed = NA_real_,
            previous_day_admission_pediatric_covid_confirmed = NA_real_
          )
        )
      }
    }

    result <- last_weekly
  }

  # calculate incidence column, change date to previous day, and
  # rename state to abbreviation
  result$data[[1]] <- result$data[[1]] %>%
    dplyr::transmute(
      abbreviation = state,
      date = date - 1,
      inc = previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
    )

  # add US location by summing across all others
  result$data[[1]] <- dplyr::bind_rows(
    result$data[[1]],
    result$data[[1]] %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(inc = sum(inc)) %>%
      dplyr::mutate(abbreviation = "US")
  )

  # add location column, remove abbreviation
  result$data[[1]] %>%
    dplyr::left_join(
      covidData::fips_codes %>% dplyr::select(location, abbreviation),
      by = "abbreviation"
    ) %>%
    dplyr::select(-abbreviation)
}

# hospitalizations
timeseries_files <- Sys.glob("data-raw/healthdata/timeseries/*.csv")
daily_files <- Sys.glob("data-raw/healthdata/daily/*.csv")

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
  data = purrr::map(
    daily_files,
    function(filename) suppressMessages(readr::read_csv(filename)))
)

all_issue_dates <- c(
  healthdata_hosp_data_ts$issue_date,
  healthdata_hosp_data_daily$issue_date
) %>%
  unique() %>%
  sort()

healthdata_hosp_data <- tibble::tibble(
  issue_date = all_issue_dates,
  data = purrr::map(
    all_issue_dates,
    build_data,
    healthdata_hosp_data_ts = healthdata_hosp_data_ts,
    healthdata_hosp_data_daily = healthdata_hosp_data_daily
  )
)

save(healthdata_hosp_data,
  file = "data/healthdata_hosp_data.rdata")
