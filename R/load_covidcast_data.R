#' Assemble a data frame of incident hospitalizations due to
#' COVID-19 as they were available as of one or more past dates in COVIDcast.
#' 
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param as_of not using this parameter now
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: state' and/or 'national'
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' must be 'hospitalizations'
#' 
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_covidcast_data <- function(issue_date = NULL, 
                                as_of = NULL,
                                spatial_resolution = "state", 
                                temporal_resolution = "weekly",
                                measure = "hospitalizations") {
  
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c("hospitalizations"))
  
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
  
  if (issue_date < "2020-11-16"){
    warning("Warning in load_covidcast_data: The earliest issue_date is 2020-11-16.
            This function will load the latest instead.")
    issue_date <- NULL
  }
  
  # create geo_type based on spatial resolution
  if ("state" %in% spatial_resolution) {
    geo_type <- spatial_resolution
  }
  
  if ("national" %in% spatial_resolution) {
    geo_type <- replace(spatial_resolution, spatial_resolution =="national", "us")
  }
  
  # loading data from covidcast and return selected columns
  results <- suppressMessages(
    covidcast::covidcast_signal(data_source = "hhs", 
                                signal = "confirmed_admissions_covid_1d",
                                as_of = issue_date,
                                geo_type = geo_type)) %>% 
    dplyr::mutate(geo_value = toupper(geo_value)) %>%
    dplyr::left_join(covidData::fips_codes, 
                     by = c("geo_value" = "abbreviation")) %>%
    dplyr::rename(date = time_value, inc = value) %>%
    dplyr::select(date, inc, location)
  
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
      dplyr::summarize(inc = sum(inc, na.rm = FALSE)) %>%
      dplyr::ungroup()
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
  
}