#' Assemble a data frame of incident and cumulative cases, deaths or hospitalizations due to
#' COVID-19 as they were available as of one or more past dates.
#'
#' @param location_code character vector of location codes. Default to NULL.
#' For US locations, this should be a list of FIPS code or 'US' 
#' For ECDC locations, this should be a list of location name abbreviation.
#' @param issues vector of issue dates (i.e. report dates) to use for querying data,
#' either \code{Date} objects or strings in the format 'yyyy-mm-dd'. Data for the
#' requested measures that were reported or updated exactly on the specified
#' issue date(s) will be returned. If multiple issue dates are provided, the result
#' includes the data for all such issue dates.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format 'yyyy-mm-dd'. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given \code{as_of} date are returned.
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: one or more of 'county', 'state' and/or 'national'.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of 'daily' or 'weekly'
#' @param measure string specifying measure of covid dynamics:
#' one of 'deaths', 'cases', or 'hospitalizations'
#' @param geography character, which data to read. Default is "US", other option is
#' "global"
#' @param source string specifying data source.  Currently supported sources are
#' "jhu" for the "deaths" or "cases" measures or "healthdata" for the "hospitalizations"
#' measure.
#'
#' @return data frame with columns location (fips code), date, inc, cum, issue_date, as_of
#'
#' @details Data for a specified \code{issue} are only returned if the data were first available
#' on that date, or were updated on that date. A warning is generated for any issue dates
#' for which no data were available.
#'
#' A query based on an \code{as_of} date returns the data for the most recent
#' \code{issue} date that is on or before the specified \code{as_of} date.
#' A warning is generated for any \code{as_of} dates for which no data were
#' available; this only occurs if the \code{as_of} date is prior to any data release for the
#' specified measure.
#'
#' If the user provides values for both \code{issue} and as_of, a warning is generated
#' and the argument for \code{issue} is ignored.
#'
#' If multiple \code{issue} dates or \code{as_of} dates are provided, the result combines
#' the data for all such dates. If no value is provided for either \code{issue} or \code{as_of},
#' results for the most recent available \code{as_of} date are returned.
#'
#' @export
load_data <- function(issues = NULL,
                      as_of = NULL,
                      location_code = NULL,
                      spatial_resolution = "state",
                      temporal_resolution = "weekly",
                      measure = "deaths",
                      geography = c("US", "global"),
                      source = NULL) {

  # validate measure
  measure <- match.arg(
    measure,
    choices = c("deaths", "cases", "hospitalizations"),
    several.ok = FALSE
  )

  # validate spatial_resolution
  # hospitalizations measure doesn't allow for spatial resolution of county
  if (measure == "hospitalizations") {
    spatial_resolution <- match.arg(
      spatial_resolution,
      choices = c("state", "national"),
      several.ok = TRUE
    )
  } else {
    spatial_resolution <- match.arg(
      spatial_resolution,
      choices = c("county", "state", "national"),
      several.ok = TRUE
    )
  }

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c("daily", "weekly"),
    several.ok = FALSE
  )

  # validate source
  if (is.null(source)) {
    source <- ifelse(measure == "hospitalizations", "healthdata", "jhu")
  } else {
    source <- match.arg(
      source,
      choices = c("healthdata", "jhu"),
      several.ok = FALSE
    )
  }
  if (measure == "hospitalizations" && source != "healthdata") {
    stop("Source must be 'healthdata' when measure is 'hospitalizations'.")
  }
  if (measure != "hospitalizations" && source == "healthdata") {
    stop("Source must be 'jhu' when measure is 'cases' or 'deaths'.")
  }

  # validate issues and as_of
  if (!is.null(issues) && !is.null(as_of)) {
    warning("Cannot provide both arguments issues and as_of to load_data. Ignoring the issues argument.")
    issues <- NULL
  }
  
  # source proper function
  if (measure == "hospitalizations") {
    function_call <- covidData::load_healthdata_data
    geography <- c("US")
  } else {
    function_call <- covidData::load_jhu_data
  }

  # return results
  if (!is.null(issues)) {
    purrr::map_dfr(issues,
      function_call,
      as_of = as_of,
      location_code = location_code,
      spatial_resolution = spatial_resolution,
      temporal_resolution = temporal_resolution,
      measure = measure,
      geography = geography
    )
  } else if (!is.null(as_of)) {
    purrr::map_dfr(as_of, 
        function_call,
        issue_date = issues,
        location_code = location_code,
        spatial_resolution = spatial_resolution,
        temporal_resolution = temporal_resolution,
        measure = measure,
        geography = geography
    )
  } else {
    function_call(
      issue_date = issues, 
      as_of = as_of,
      location_code = location_code,
      spatial_resolution = spatial_resolution,
      temporal_resolution = temporal_resolution,
      measure = measure, 
      geography = geography
    )
  }
}
