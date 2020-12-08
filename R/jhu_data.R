#' Assemble a data frame of incident and cumulative deaths or cases due to
#' COVID-19 as they were available as of a specified issue date.
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state' and/or 'national'
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @param replace_negatives boolean to replace negative incs with imputed data
#' @param adjustment_cases character vector specifying times and locations with
#' reporting anomalies to adjust.  Either 'none' (the default) or one or more
#' of 'CO-2020-04-24', 'MS-2020-06-22', 'DE-2020-06-23', 'NJ-2020-06-25'. These
#' refer to locations and times affected by reporting anomalies documented at
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#user-content-retrospective-reporting-of-probable-cases-and-deaths
#' @param adjustment_method string specifying how anomalies are adjusted.
#' 'fill_na' will replace affected observations with NAs and correct daily
#' cumulative counts for all dates on and after adjustment date.
#' 'impute_and_redistribute' will replace affected observations with imputed values.
#' Difference between the original observation and the imputed value will be redistributed
#' to observations before and on the adjustment date.
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_jhu_data <- function(
    issue_date = NULL,
    as_of = NULL,
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "deaths",
    replace_negatives = FALSE,
    adjustment_cases = "none",
    adjustment_method = "none") {
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c("cases", "deaths"))
  if (measure == "cases") {
    jhu_data <- covidData::jhu_cases_data
  } else if (measure == "deaths") {
    jhu_data <- covidData::jhu_deaths_data
  }

  # validate issue_date and as_of
  if (!missing(issue_date) && !missing(as_of) &&
      !is.null(issue_date) && !is.null(as_of)) {
    stop("Cannot provide both arguments issue_date and as_of to load_jhu_data.")
  } else if (is.null(issue_date) && is.null(as_of)) {
    issue_date <- max(jhu_data$issue_date)
  } else if (!is.null(as_of)) {
    avail_issues <- jhu_data$issue_date[
        jhu_data$issue_date <= as.character(as_of)
      ]

    if (length(avail_issues) == 0) {
      stop("Provided as_of date is earlier than all available issue dates.")
    } else {
      issue_date <- max(avail_issues)
    }
  } else {
    issue_date <- as.character(lubridate::ymd(issue_date))
  }

  if (!(issue_date %in% jhu_data$issue_date)) {
    stop(paste0(
      "Invalid issue date; must be one of: ",
      paste0(jhu_data$issue_date, collapse = ", ")
    ))
  }

  # validate spatial_resolution
  spatial_resolution <- match.arg(
    spatial_resolution,
    choices = c("county", "state", "national"),
    several.ok = TRUE
  )

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c("daily", "weekly"),
    several.ok = FALSE
  )

  adjustment_method <- match.arg(
    adjustment_method,
    choices = c("fill_na", "impute_and_redistribute", "none"),
    several.ok = FALSE
  )

  # get report for specified issue date
  jhu_data <- jhu_data %>%
    dplyr::filter(issue_date == UQ(issue_date)) %>%
    dplyr::pull(data) %>%
    `[[`(1) %>%
    tidyr::pivot_longer(
      matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
      names_to = "date",
      values_to = "cum"
    ) %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))
    )

  # summarized results for county level
  results <- NULL
  if ("county" %in% spatial_resolution) {
    county_results <- jhu_data %>%
      dplyr::filter(FIPS > 100) %>%
      dplyr::mutate(
        location = sprintf("%05d", FIPS)
      ) %>%
      dplyr::filter(location < "80001") %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(inc = diff(c(0, cum))) %>%
      dplyr::select(location, date, cum, inc) %>%
      dplyr::ungroup()

    results <- dplyr::bind_rows(results, county_results)
  }

  # summarized results for state level
  if ("state" %in% spatial_resolution) {
    states_to_keep <- c(
      "Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas",
      "California", "Colorado", "Connecticut", "Delaware",
      "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho",
      "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
      "Maryland", "Massachusetts", "Michigan", "Minnesota",
      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
      "New Hampshire", "New Jersey", "New Mexico", "New York",
      "North Carolina", "North Dakota", "Northern Mariana Islands",
      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico",
      "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
      "Texas", "Utah", "Vermont", "Virgin Islands", "Virginia",
      "Washington", "West Virginia", "Wisconsin", "Wyoming"
    )

    state_results <- jhu_data %>%
      dplyr::filter(Province_State %in% states_to_keep) %>%
      dplyr::mutate(location_name = Province_State) %>%
      dplyr::group_by(location_name, date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::group_by(location_name) %>%
      dplyr::mutate(inc = diff(c(0, cum))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        covidData::fips_codes %>% dplyr::filter(nchar(location) == 2),
        by = "location_name"
      ) %>%
      dplyr::select(location, date, cum, inc)

    results <- dplyr::bind_rows(results, state_results)
  }

  # summarized results for national level
  if ("national" %in% spatial_resolution) {
    # because we don't filter on states_to_keep as above, we are off by a total
    # of 3 deaths attributed to Diamond Princess.
    national_results <- jhu_data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        inc = diff(c(0, cum)),
        location = "US"
      ) %>%
      dplyr::select(location, date, cum, inc)

    results <- dplyr::bind_rows(results, national_results)
  }

  # replace negative incidence with imputed data. Residuals will be
  # redistributed to related observations.
  if (replace_negatives) {
    results <- replace_negatives(data = results, measure = measure)
  }

  if (adjustment_cases != "none" & length(adjustment_cases) > 0) {
    # create a data frame with adjustment location fips code and adjustment date
    adjustment_states <- sub("-.*", "", adjustment_cases)
    adjustment_dates <- sub("^.*?-", "", adjustment_cases)
    adjustment_state_fips <- purrr::map_chr(
      adjustment_states, function(x) {
        fips_codes[which(covidData::fips_codes$abbreviation == x), ]$location
      }
    )
    adjustments <- data.frame(
      location = adjustment_state_fips,
      date = as.Date(adjustment_dates)
    )

    # replace daily incidence with NA in specific rows
    if ("fill_na" %in% adjustment_method) {
      results <- fill_na(
        results = results,
        adjustments = adjustments
      )
    }

    # replace daily incidence with imputed data and redistribute
    # residuals to related observations
    if ("impute_and_redistribute" %in% adjustment_method) {
       if (replace_negatives == FALSE) {
         results = replace_negatives(data = results, measure = measure)

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

      for (i in seq_len(nrow(adjustments))) {
        adjustment_location <- adjustments[i, ]$location
        adjustment_date <- as.Date(adjustments[i, ]$date)

        # get state, counties and national observations for an adjustment case
        location_data <- results %>%
          dplyr::filter(
            stringr::str_sub(location, start = 1, end = 2) %in%
              adjustment_location |
            location == "US" | location == adjustment_location)

        # for each location in data, get imputed data
        for (fips in unique(location_data$location)) {
          d <- location_data[location_data$location == fips, ]

          # get adjusted inc column
          imputed_inc <- adjust_daily_incidence(
            d,
            adjustment_date,
            measure = measure
          )

          # put imputed data back to results
          results[which(results$location == fips), ]$inc <- imputed_inc
        }
      }
    }
  }




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

  return(results)
}
