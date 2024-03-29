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
#' Default to 'state'.
#' Note that 'county' is not available for hospitalization data.
#' When source is "covidcast", this parameter has to match with location_code, if specified.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of 'daily' or 'weekly'
#' @param measure string specifying measure of disease dynamics:
#' one of 'deaths', 'cases', 'hospitalizations', or 'flu hospitalizations'.
#' The first three of these refer to measures of covid intensity.
#' Default to 'deaths'.
#' @param geography character, which data to read. Default is "US", other option is
#' "global".
#' Note that "global" is not available for hospitalization data and "covidcast" source.
#' @param source string specifying data source.  Currently supported sources are
#' "jhu" or "covidcast" for the "deaths" or "cases" measures;
#' "healthdata" or "covidcast" for the "hospitalizations" and
#' "flu hospitalizations" measures.
#' Default to NULL which means "healthdata" for hospitalization data and "jhu"
#' for all other measures.
#' @param drop_last_date boolean indicating whether to drop the last 1 day of
#' data for the influenza and COVID hospitalization signals. The last day of
#' data from the HHS data source is unreliable, so it is recommended to set this
#' to `TRUE`. However, the default is `FALSE` so that the function maintains
#' fidelity to the authoritative data source. This argument is ignored if the
#' `measure` is 'deaths' or 'cases'.
#'
#' @return data frame with columns location (fips code), date, inc, cum
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
                      source = NULL,
                      drop_last_date = FALSE) {
  # validate measure
  measure <- match.arg(
    measure,
    choices = c("deaths", "cases", "hospitalizations", "flu hospitalizations"),
    several.ok = FALSE
  )

  # validate spatial_resolution
  # hospitalizations measure doesn't allow for spatial resolution of county
  if (measure %in% c("hospitalizations", "flu hospitalizations")) {
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
    source <- ifelse(
        measure %in% c("hospitalizations", "flu hospitalizations"),
        "healthdata",
        "jhu")
  } else {
    source <- match.arg(
      source,
      choices = c("healthdata", "jhu", "covidcast"),
      several.ok = FALSE
    )
  }

  if (measure %in% c("hospitalizations", "flu hospitalizations") &&
    !source %in% c("healthdata", "covidcast")) {
    stop("Source must be 'healthdata' or 'covidcast' when measure is ",
         "'hospitalizations' or 'flu hospitalizations'.")
  } else if (!(measure %in% c("hospitalizations", "flu hospitalizations")) &&
             source == "healthdata") {
    stop("Source must be 'jhu' or 'covidcast' when measure is not ",
         "'hospitalizations' or 'flu hospitalizations'.")
  }
  
  if (measure %in% c("hospitalizations", "flu hospitalizations") &&
      geography[1] != "US") {
    geography <- "US"
    warning("Only US hospitalization data are available now. Will be loading US data instead.")
  }

  # validate issues and as_of
  if (!is.null(issues) && !is.null(as_of)) {
    warning("Cannot provide both arguments issues and as_of to load_data. Ignoring the issues argument.")
    issues <- NULL
  }

  # source proper function
  if (source == "healthdata") {
    function_call <- covidData::load_healthdata_data
  } else if (source == "covidcast") {
    function_call <- covidData::load_covidcast_data
  } else if (source == "jhu") {
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
      geography = geography,
      drop_last_date = drop_last_date
    )
  } else if (!is.null(as_of)) {
    purrr::map_dfr(as_of,
      function_call,
      issue_date = issues,
      location_code = location_code,
      spatial_resolution = spatial_resolution,
      temporal_resolution = temporal_resolution,
      measure = measure,
      geography = geography,
      drop_last_date = drop_last_date
    )
  } else {
    function_call(
      issue_date = issues,
      as_of = as_of,
      location_code = location_code,
      spatial_resolution = spatial_resolution,
      temporal_resolution = temporal_resolution,
      measure = measure,
      geography = geography,
      drop_last_date = drop_last_date
    )
  }
}
