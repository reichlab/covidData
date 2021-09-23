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
#' include: 'state', 'national' and/or 'county'.
#' It has to match with locations in `location_code`. 
#' Default to NULL, all available spatial resolution based on the given measure. 
#' Note that 'county' is not available for hospitalization measure.
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' must be one of 'hospitalizations', 'deaths' or 'cases'.
#' @param geography character, which data to read. Default is "US".
#' Note this variable is not used in the function
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_covidcast_data <- function(issue_date = NULL,
                                as_of = NULL,
                                location_code = NULL,
                                spatial_resolution = NULL,
                                temporal_resolution = "weekly",
                                measure = "hospitalizations",
                                geography = "US") {

  # validate measure and pull in correct data set
  measure <- match.arg(
    measure,
    choices = c("hospitalizations", "cases", "deaths"),
    several.ok = FALSE
  )

  # set covidcast signal and data source based on measure
  if (measure == "hospitalizations") {
    signal <- "confirmed_admissions_covid_1d"
    data_source <- "hhs"
    geo_type_choices <- c("nation", "state")
  } else if (measure == "cases") {
    signal <- "confirmed_incidence_num"
    data_source <- "jhu-csse"
    geo_type_choices <- c("nation", "state", "county")
  } else if (measure == "deaths") {
    signal <- "deaths_incidence_num"
    data_source <- "jhu-csse"
    geo_type_choices <- c("nation", "state", "county")
  }

  # validate location_code
  if (!is.null(location_code)) {
    location_code <- match.arg(
      location_code,
      choices = covidData::fips_codes$location,
      several.ok = TRUE
    )
    
    location_info <- covidData::fips_codes %>%
      dplyr::filter(location %in% location_code)
  }

  # generate corresponding geo_types based on location_code
  if (!is.null(location_code)) {
    geo_type_choices <- unique(location_info$geo_type)
  } 

  # validate spatial_resolution
  if (!is.null(spatial_resolution)) {
    spatial_resolution <- replace(spatial_resolution, spatial_resolution == "national", "nation")
    if (!is.null(location_code)) {
      if (!dplyr::setequal(spatial_resolution, geo_type_choices)) {
        stop("Please make sure spatial_resolution matches with location_code.")
      }
    } else {
      spatial_resolution <- match.arg(
        spatial_resolution,
        choices = geo_type_choices,
        several.ok = TRUE
      )
    }
  } else {
    spatial_resolution <- geo_type_choices
  }

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c("daily", "weekly"),
    several.ok = FALSE
  )

  # warnings for issue date
  if (!is.null(issue_date)) {
    if (measure == "hospitalizations" & issue_date < "2020-11-16") {
      warning("Warning in load_covidcast_data: The earliest issue_date for hospitalization data is 2020-11-16.
              This function will load the latest instead.")
      issue_date <- NULL
    } else if (measure != "hospitalizations" & issue_date < "2020-05-07") {
      warning("Warning in load_covidcast_data: The earliest issue_date for death/case data is 2020-05-07.
              This function will load the latest instead.")
      issue_date <- NULL
    }
  }

  # loading data from covidcast and return selected columns
  results <- purrr::map_dfr(
    spatial_resolution,
    function(curr_geo_type) {
      if (curr_geo_type != "county" | measure != "hospitalizations") {
        if (curr_geo_type == "nation") {
          curr_geo_value <- "*"
        } else {
          # "state" and "county"
          if (is.null(location_code)) {
            curr_geo_value <- "*"
          } else {
            curr_geo_value <- location_info[location_info$geo_type == curr_geo_type,]$geo_value
          }
        }

        suppressMessages(
          covidcast::covidcast_signal(
            data_source = data_source,
            signal = signal,
            as_of = as_of,
            issues = issue_date,
            geo_type = curr_geo_type,
            geo_value = curr_geo_value) %>%
            dplyr::left_join(covidData::fips_codes,
              by = c("geo_value" = "geo_value")) %>%
            dplyr::rename(date = time_value, inc = value) %>%
            # keep fips code
            dplyr::select(date, inc, location)
        )
      }
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
