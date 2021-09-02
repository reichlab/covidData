#' Assemble a data frame of incident hospitalizations due to
#' COVID-19 as they were available as of a specified issue date in COVIDcast.
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param as_of character as_of date to use for constructing truths in
#' format 'yyyy-mm-dd'
#' @param location_code character vector of location codes. Default to NULL.
#' For US locations, this should be a list of FIPS code or 'US'
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'state' and/or 'national'. It has to match with locations in `location_code`.
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' must be 'hospitalizations'
#' @param geography character, which data to read. Default is "US".
#' Note this variable is not used in the function
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_covidcast_data <- function(issue_date = NULL,
                                as_of = NULL,
                                location_code = NULL,
                                spatial_resolution = "state",
                                temporal_resolution = "weekly",
                                measure = "hospitalizations",
                                geography = "US") {

  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c("hospitalizations"))

  if (!is.null(location_code)) {
    location_code <- match.arg(
      location_code,
      choices = covidData::fips_codes$location,
      several.ok = TRUE
    )

    # match location fips code to lower-cased abbreviations
    location_code <- covidData::fips_codes %>%
      dplyr::filter(location %in% location_code) %>%
      dplyr::pull(abbreviation)
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

  if (!is.null(issue_date)) {
    if (issue_date < "2020-11-16") {
      warning("Warning in load_covidcast_data: The earliest issue_date is 2020-11-16.
              This function will load the latest instead.")
      issue_date <- NULL
    }
  }

  geo_type <- NULL
  # create geo_type based on spatial resolution
  
  if (!is.null(location_code)) {
    if (("US" %in% location_code && !"national" %in% spatial_resolution)||
        (!"US" %in% location_code && !"state" %in% spatial_resolution)){
      stop("Error in load_covidcast_data: Cannot load data for requested location_code with current spatial_resolution.")
    }
  }
  

  geo_type <- replace(spatial_resolution, spatial_resolution == "national", "nation")

  # loading data from covidcast and return selected columns
  results <- purrr::map_dfr(
    geo_type,
    function(curr_geo_type) {
      if (curr_geo_type == "nation") {
        # default in covidcast_signal
        curr_geo_value <- "*"
      } else if (curr_geo_type == "state") {
        if (is.null(location_code)) {
          curr_geo_value <- "*"
        } else {
          curr_geo_value <- tolower(location_code[location_code != "US"])
        }
      }
      suppressMessages(
        covidcast::covidcast_signal(
          data_source = "hhs",
          signal = "confirmed_admissions_covid_1d",
          as_of = as_of,
          issues = issue_date,
          geo_type = curr_geo_type,
          geo_value = curr_geo_value
        ) %>%
          dplyr::mutate(geo_value = toupper(geo_value)) %>%
          dplyr::left_join(covidData::fips_codes,
            by = c("geo_value" = "abbreviation")
          ) %>%
          dplyr::rename(date = time_value, inc = value) %>%
          # keep fips code
          dplyr::select(date, inc, location)
      )
    }
  )

  # aggregate daily incidence to weekly incidence
  if (temporal_resolution == "weekly") {
    results <- results %>%
      dplyr::mutate(
        sat_date = lubridate::ceiling_date(
          lubridate::ymd(date),
          unit = "week"
        ) - 1
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

  return(results)
}
