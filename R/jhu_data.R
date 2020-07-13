#' Construct table of truths for weekly incident and cumulative deaths at
#' horizons one though 6
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state' and/or 'national'
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: currently only 'weekly' is supported
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @param adjustment_cases character vector specifying times and locations with
#' reporting anomalies to adjust.  Either 'none' (the default) or one or more
#' of 'CO-2020-04-24', 'MS-2020-06-22', 'DE-2020-06-23', 'NJ-2020-06-25'. These
#' refer to locations and times affected by reporting anomalies documented at
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#user-content-retrospective-reporting-of-probable-cases-and-deaths
#' @param adjustment_method string specifying how anomalies are adjusted.
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_jhu_data <- function(
  issue_date = NULL,
  spatial_resolution = 'state',
  temporal_resolution = 'weekly',
  measure = 'deaths',
  adjusted = FALSE
  ) {
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c('cases', 'deaths'))
  if(measure == 'cases') {
    jhu_data <- covidData::jhu_cases_data
  } else if(measure == 'deaths') {
    jhu_data <- covidData::jhu_deaths_data
  }
  
  # validate issue_date
  if(is.null(issue_date)) {
    issue_date <- max(jhu_data$issue_date)
  } else {
    issue_date <- as.character(lubridate::ymd(issue_date))
  }
  if(!(issue_date %in% jhu_data$issue_date)) {
    stop(paste0('Invalid issue date; must be one of: ',
                paste0(jhu_data$issue_date, collapse = ', ')))
  }

  # validate spatial_resolution
  spatial_resolution <- match.arg(
    spatial_resolution,
    choices = c('county', 'state', 'national'),
    several.ok = TRUE)

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c('daily', 'weekly'),
    several.ok = FALSE
  )

  # get report for specified issue date
  jhu_data <- jhu_data %>%
    dplyr::filter(issue_date == UQ(issue_date)) %>%
    dplyr::pull(data) %>%
    `[[`(1) %>%
    tidyr::pivot_longer(
      matches('^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$'),
      names_to = 'date',
      values_to = 'cum') %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))
    )

  # if weekly temporal resolution, filter to saturdays
  if(temporal_resolution == 'weekly') {
    jhu_data <- jhu_data %>%
      dplyr::filter(
        lubridate::wday(lubridate::ymd(date), label = TRUE) == 'Sat'
      )
  }

  # summarized results for county level
  results <- NULL
  if('county' %in% spatial_resolution) {
    county_results <- jhu_data %>%
      dplyr::filter(FIPS > 100) %>%
      dplyr::mutate(
        location = sprintf("%05d", FIPS)) %>%
      dplyr::filter(location < '80001') %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(inc = cum - dplyr::lag(cum, 1L)) %>%
      dplyr::select(location, date, cum, inc) %>%
      dplyr::ungroup()
    
    results <- dplyr::bind_rows(results, county_results)
  }
  
  # summarized results for state level
  if('state' %in% spatial_resolution) {
    states_to_keep <- c(
      'Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California',
      'Colorado', 'Connecticut', 'Delaware', 'District of Columbia',
      'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois',
      'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
      'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',
      'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada',
      'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
      'North Carolina', 'North Dakota', 'Northern Mariana Islands',
      'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico',
      'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
      'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia',
      'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

    state_results <- jhu_data %>%
      dplyr::filter(Province_State %in% states_to_keep) %>%
      dplyr::mutate(location_name = Province_State) %>%
      dplyr::group_by(location_name, date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::group_by(location_name) %>%
      dplyr::mutate(inc = cum - dplyr::lag(cum, 1L)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(covidEnsembles::fips_codes, by = 'location_name') %>%
      dplyr::select(location, date, cum, inc)
    
    results <- dplyr::bind_rows(results, state_results)
  }

  # summarized results for national level
  if('national' %in% spatial_resolution) {
    # because we don't filter on states_to_keep as above, we are off by a total
    # of 3 deaths attributed to Diamond Princess.
    national_results <- jhu_data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        inc = cum - dplyr::lag(cum, 1L),
        location = 'US'
      ) %>%
      dplyr::select(location, date, cum, inc)

    results <- dplyr::bind_rows(results, national_results)
  }

  return(results)
}
