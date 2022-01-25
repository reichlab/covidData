#' Assemble a data frame of incident hospitalizations due to
#' COVID-19 as they were available as of a specified issue date.
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param location_code character vector of location codes. Default to NULL
#' This should be a list of state FIPS code and/or 'US'. 
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: state' and/or 'national'
#' This parameter will be ignored if location_code is provided.
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' must be 'hospitalizations'
#' @param replace_negatives boolean to replace negative incs with imputed data
#' Currently only FALSE is supported
#' @param adjustment_cases character vector specifying times and locations with
#' reporting anomalies to adjust.  Only the value "none" is currently supported
#' @param adjustment_method string specifying how anomalies are adjusted.
#' Only the value "none" is currently supported.
#' @param geography character, which data to read. Only "US" is supported.
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#' all values of cum will currently be NA
#'
#' @export
load_healthdata_data <- function(
    issue_date = NULL,
    as_of = NULL,
    location_code = NULL,
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "hospitalizations",
    replace_negatives = FALSE,
    adjustment_cases = "none",
    adjustment_method = "none",
    geography = "US") {
  
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c("hospitalizations"))

  #retrieve data update history
  healthdata_timeseries_history <- healthdata_timeseries_history()
  healthdata_dailyrevision_history <- healthdata_dailyrevision_history()
  
  all_avail_issue_date <- unique(c(healthdata_timeseries_history$issue_date,
                                   healthdata_dailyrevision_history$issue_date))
  all_avail_issue_date <- unique(c(all_avail_issue_date,
                                   covidData::healthdata_hosp_early_data$issue_date))

  # validate issue_date and as_of
  if (!is.null(issue_date) && !is.null(as_of)) {
    stop("Cannot provide both arguments issue_date and as_of to load_healthcare_data.")
  } else if (is.null(issue_date) && is.null(as_of)) {
    issue_date <- max(all_avail_issue_date)
  } else if (!is.null(as_of)) {
    avail_issues <- all_avail_issue_date[
      all_avail_issue_date <= as.character(as_of)
      ]

    if (length(avail_issues) == 0) {
      stop("Provided as_of date is earlier than all available issue dates.")
    } else {
      issue_date <- max(avail_issues)
    }
  } else {
    issue_date <- as.character(lubridate::ymd(issue_date))
  }

  if (!(issue_date %in% all_avail_issue_date)) {
    stop(paste0(
      'Invalid issue date; must be one of: ',
      paste0(all_avail_issue_date, collapse = ', ')
    ))
  }

  # validate spatial_resolution
  spatial_resolution <- match.arg(
    spatial_resolution,
    choices = c("state", "national"),
    several.ok = TRUE
  )

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c("daily", "weekly"),
    several.ok = FALSE
  )

  # validate replace_negatives
  if (replace_negatives) {
    stop("Currently, only replace_negatives = FALSE is supported")
  }

  # validate adjustment_cases and adjustment_method
  adjustment_cases <- match.arg(
    adjustment_cases,
    choices = "none",
    several.ok = FALSE
  )

  adjustment_method <- match.arg(
    adjustment_method,
    choices = "none",
    several.ok = FALSE
  )

  #download and preprocess data based on issue_date
  raw_healthdata_data <- build_healthdata_data(
    issue_date,
    healthdata_timeseries_history,
    healthdata_dailyrevision_history)
  
  healthdata_data <- preprocess_healthdata_data(
    raw_healthdata_data,
    covidData::fips_codes) %>%
    dplyr::pull(data) %>%
    `[[`(1)
  
  all_locations <- unique(healthdata_data$location)
  
  if (!is.null(location_code)){
    locations_to_keep <- match.arg(
      location_code, 
      choices = all_locations,
      several.ok = TRUE)
    
    # ignore spatial_resolution
    spatial_resolution <- NULL
  } else {
    # drop results for irrelevant locations
    locations_to_keep <- NULL
    if ("state" %in% spatial_resolution) {
      locations_to_keep <- all_locations[all_locations != "US"]
    }
    
    if ("national" %in% spatial_resolution) {
      locations_to_keep <- c(locations_to_keep, "US")
    }
  }

  results <- healthdata_data %>%
    dplyr::select(location, date, inc) %>%
    dplyr::filter(location %in% locations_to_keep)

  # aggregate daily incidence to weekly incidence
  if (temporal_resolution == "weekly") {
    results <- results %>%
      dplyr::mutate(
        sat_date = lubridate::ceiling_date(
          lubridate::ymd(date), unit = "week") - 1
      ) %>%
      dplyr::group_by(location) %>%
      # if the last week is not complete, drop all observations from the
      # previous Saturday in that week
      dplyr::filter(
        if (max(date) < max(sat_date)) date <= max(sat_date) - 7 else TRUE
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-date) %>%
      dplyr::rename(date = sat_date) %>%
      dplyr::group_by(location, date) %>%
      dplyr::summarize(inc = sum(inc, na.rm = FALSE), .groups = "drop")
  }
  
  # aggregate inc to get the correct cum
  results <- results %>%
    dplyr::mutate(
      date = lubridate::ymd(date),
      cum = results %>%
        dplyr::group_by(location) %>%
        dplyr::mutate(cum = cumsum(inc)) %>%
        dplyr::ungroup() %>%
        dplyr::pull(cum)
    )

  return(results)
}

#' Preprocess healthdata data set, calculating incidence, adjusting date, and
#' calculating national incidence.
#' 
#' @param raw_healthdata_data tibble one row and columns issue_date and data
#' The data column should be a list of data frames, with column names
#' date, state, previous_day_admission_adult_covid_confirmed, and
#' previous_day_admission_pediatric_covid_confirmed
#' 
#' @param fips_codes covidData::fips_codes data object
#' @return a result of similar format to raw_healthdata_data, but columns
#' date, location, and inc
preprocess_healthdata_data <- function(raw_healthdata_data, fips_codes) {
  result <- raw_healthdata_data

  # calculate incidence column, change date to previous day, and
  # rename state to abbreviation
  result$data[[1]] <- result$data[[1]] %>%
    dplyr::transmute(
      abbreviation = state,
      date = as.Date(date) - 1,
      inc = previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
    )

  # add US location by summing across all others
  result$data[[1]] <- dplyr::bind_rows(
    result$data[[1]],
    result$data[[1]] %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(inc = sum(inc), .groups = "drop") %>%
      dplyr::mutate(abbreviation = "US")
  )

  # add location column, remove abbreviation
  result$data[[1]] <- result$data[[1]] %>%
    dplyr::left_join(
      fips_codes %>% dplyr::select(location, abbreviation),
      by = "abbreviation"
    ) %>%
    dplyr::select(-abbreviation)
  
  return(result)
}
