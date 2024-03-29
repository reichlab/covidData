#' Assemble a data frame of incident and cumulative deaths or cases due to
#' COVID-19 as they were available as of a specified issue date.
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param location_code character vector of location codes. Default to NULL.
#' For US locations, this should be a list of FIPS code or 'US' 
#' For ECDC locations, this should be a list of location name abbreviation.
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state' and/or 'national'.
#' This parameter will be ignored if location_code is provided or geography is "global".
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: 'daily' or 'weekly'
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @param geography character, which data to read. Default is "US", other option is
#' "global"
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
#' @param ... ignored
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_jhu_data <- function(
    issue_date = NULL,
    as_of = NULL,
    location_code = NULL,
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "deaths",
    geography = c("US", "global"),
    replace_negatives = FALSE,
    adjustment_cases = "none",
    adjustment_method = "none",
    ...) {
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c("cases", "deaths"))
  
  # validate issue_date and as_of and load preprocessed data
  jhu_data <- preprocess_jhu_data(issue_date, as_of, measure, geography)
  
  if (geography[1] == "US"){
    valid_locations <- covidData::fips_codes
  } else if (geography[1] == "global"){
    valid_locations <- covidData::global_locations
  }
  
  # validate location_code
  if (!is.null(location_code)){
    location_code <- match.arg(
      location_code, 
      choices = valid_locations$location, 
      several.ok = TRUE)
    
    # ignore spatial_resolution
    spatial_resolution <- NULL
  } else {
    # validate spatial_resolution
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
  
  # validate adjustment_method
  adjustment_method <- match.arg(
    adjustment_method,
    choices = c("fill_na", "impute_and_redistribute", "none"),
    several.ok = FALSE
  )
  

  # get report for specified issue date
  jhu_data <- jhu_data %>%
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
  
  selected_counties <- NULL
  selected_states <- NULL
  selected_us <- NULL
  country_names <- NULL
  
  # create location filters based on location_code and geography parameters
  if (!is.null(location_code)){
    if (geography[1] == "US"){
      # create sublists based on location_code
      selected_counties <- location_code[lapply(location_code, nchar) == 5]
      selected_states <- location_code[lapply(location_code, nchar) == 2]
      if ("US" %in% selected_states) {
        selected_states[selected_states != "US"]
        selected_us <- c("US")
      }
      
    } else if (geography[1] == "global"){
      # get corresponding country names
      country_names <- valid_locations %>%
        dplyr::filter(location %in% location_code) %>%
        dplyr::pull(location)
    }
  }
  
  # aggregate and filter based on geography parameter
  if (geography[1] == "US"){
    # summarized results for county level
    results <- NULL
      
    if ("county" %in% spatial_resolution | length(selected_counties) > 0) {
      county_results <- jhu_data %>%
        dplyr::filter(FIPS > 100) %>%
        dplyr::mutate(
          location = sprintf("%05d", FIPS)) %>%
          dplyr::filter(location < "80001") %>%
          dplyr::group_by(location) %>%
          dplyr::mutate(inc = diff(c(0, cum))) %>%
          dplyr::select(location, date, cum, inc) %>%
          dplyr::ungroup()
        
        if (length(selected_counties) > 0) {
          county_results <- county_results %>%
            dplyr::filter(location %in% selected_counties)
        }
        
        results <- dplyr::bind_rows(results, county_results)
      }
      
    # summarized results for state level
    if ("state" %in% spatial_resolution | length(selected_states) > 0 ) {
      if (length(selected_states) > 0) {
          # pull state names from fips code
          states_to_keep <- valid_locations %>%
            dplyr::filter(location %in% selected_states) %>%
            dplyr::pull(location_name)
      } else {
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
          "Washington", "West Virginia", "Wisconsin", "Wyoming")
      }
        
      state_results <- jhu_data %>%
        dplyr::filter(Province_State %in% states_to_keep) %>%
        dplyr::mutate(location_name = Province_State) %>%
        dplyr::group_by(location_name, date) %>%
        dplyr::summarize(cum = sum(cum)) %>%
        dplyr::group_by(location_name) %>%
        dplyr::mutate(inc = diff(c(0, cum))) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          valid_locations %>% dplyr::filter(nchar(location) == 2),
          by = "location_name") %>%
        dplyr::select(location, date, cum, inc)
        
      results <- dplyr::bind_rows(results, state_results)
    }
      
    # summarized results for national level
    if ("national" %in% spatial_resolution | length(selected_us) == 1) {
      # because we don't filter on states_to_keep as above, we are off by a total
      # of 3 deaths attributed to Diamond Princess.
      national_results <- jhu_data %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(cum = sum(cum)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          inc = diff(c(0, cum)),
          location = "US") %>%
        dplyr::select(location, date, cum, inc)
        
      results <- dplyr::bind_rows(results, national_results)
    }
  } else if (geography[1] == "global"){
      results <- jhu_data %>%
        dplyr::rename(location = `Country/Region`) %>%
        dplyr::filter(!location %in% c("Diamond Princess", "MS Zaandam")) %>%
        dplyr::group_by(location) %>%
        dplyr::mutate(inc = diff(c(0, cum))) %>%
        dplyr::ungroup() %>% 
        dplyr::left_join(y = valid_locations, 
                         by = c("location" = "location_name")) %>%
        dplyr::select(-location) %>%
        dplyr::rename(location = location.y) %>%
        dplyr::select(location, date, cum, inc)
    
      if (length(country_names) > 0){
      results <- results %>%
        dplyr::filter(location %in% country_names)
      }
  } 
  

  # replace negative incidence with imputed data. Residuals will be
  # redistributed to related observations.
  if (replace_negatives) {
    results <- replace_negatives(data = results, measure = measure)
  }

  if (adjustment_cases != "none" & length(adjustment_cases) > 0) {
    # create a data frame with adjustment location fips code and adjustment date
    adjustment_locations <- sub("-.*", "", adjustment_cases)
    adjustment_dates <- sub("^.*?-", "", adjustment_cases)
    adjustment_locations_code <- purrr::map_chr(
      adjustment_locations, function(x) {
        if (geography[1] == "US"){
          # get corresponding fips code
          valid_locations[which(valid_locations$abbreviation == x), ]$location
        } else if (geography[1] == "global"){
          x
        }
      }
    )
    adjustments <- data.frame(
      location = adjustment_locations_code,
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
        adjustment_location_code <- adjustments[i, ]$location
        adjustment_date <- as.Date(adjustments[i, ]$date)

        # get state, counties and national observations for an adjustment case
        location_data <- results %>%
          dplyr::filter(
            stringr::str_sub(
              # include counties in adjustment state
              location, start = 1, end = 2) %in% adjustment_location_code |
              location == "US" | 
              # US states or ECDC countries
              location == adjustment_location_code)

        # for each location in data, get imputed data
        for (a_location in unique(location_data$location)) {
          d <- location_data[location_data$location == a_location, ]

          # get adjusted inc column
          imputed_inc <- adjust_daily_incidence(
            d,
            adjustment_date,
            measure = measure
          )

          # put imputed data back to results
          results[which(results$location == a_location), ]$inc <- imputed_inc
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


#' Create a tibble of incident and cumulative deaths or cases based on a
#' specified issue date and measure. This function will also download a
#' specified time series data set if issue_date is not available in pre-built
#' data object.
#' 
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param as_of character issue date (i.e. report date) to use for
#' constructing truths published as of this date in format 'yyyy-mm-dd'
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @param geography character, which data to read. Default is "US", other option is
#' "global"
#' 
#' @return tibble with issue_date and data
#' 
#' @importFrom readr read_csv
preprocess_jhu_data <- function(issue_date = NULL, 
                                as_of = NULL, 
                                measure = "deaths",
                                geography = c("US", "global")){
  
  # create url for data update links files
  link_base <- "https://raw.githubusercontent.com/reichlab/covidData/master/data/"
  link_file_name <- paste("jhu", tolower(geography[1]), measure, "data_links", sep = "_")
  # load links
  links <- suppressMessages(readr::read_csv(url(paste0(link_base, link_file_name,".csv"))))
  
  if (measure == "deaths"){
    if (geography[1] == "US"){
      jhu_data <- covidData::jhu_us_deaths_data
    } else if (geography[1] == "global"){
      jhu_data <- covidData::jhu_global_deaths_data
    }
  } else if (measure == "cases"){
    if (geography[1] == "US"){
      jhu_data <- covidData::jhu_us_cases_data
    } else if (geography[1] == "global"){
      jhu_data <- covidData::jhu_global_cases_data
    }
  }
  
  # validate issue_date and as_of
  orig_issue_date_null <- is.null(issue_date)
  if (!is.null(issue_date) && !is.null(as_of)) {
    stop("Cannot provide both arguments issue_date and as_of to load_jhu_data.")
  } else if (is.null(issue_date) && is.null(as_of)) {
    issue_date <- Sys.Date()
  } else if (!is.null(as_of)) {
    # case0: as_of < min(links$date) --> error
    # case1: as_of >= min(links$date) 
    #      a. as_of <= max --> get largest_issue_date <= as_of
    #      b. as_of > max --> get the latest links, get largest_issue_date <= as_of
    if (as_of < min(links$date)) {
      stop("Provided as_of date is earlier than all available issue dates.")
    } else {
      if (as_of > max(links$date)) {
        # query Github API to get the first page of results
        links <- get_time_series_data_link(measure, first_page_only = FALSE, geography)
      }
      issue_date <- max(links$date[links$date <= as.character(as_of)])
    }
  } else {
    issue_date <- lubridate::ymd(issue_date)
  }
  
  # subset jhu_data based on issue_date
  if (issue_date %in% as.Date(jhu_data$issue_date)){
    jhu_data <- jhu_data %>%
      dplyr::filter(issue_date == UQ(issue_date))
  } else {
    if (!issue_date %in% links$date) {
      # query Github API to get the first page of results
      links <- get_time_series_data_link(measure, first_page_only = FALSE, geography)
      if (!issue_date %in% links$date){
        if (orig_issue_date_null) {
          # user just wants the latest available issue date
          issue_date <- max(links$date)
        } else {
          # user specifically requested an issue date that's not available
          stop("Couldn't find link to the timeseries data file. Please check issue_date parameter.")
        }
      }
    } 
    # download data from link
    link <- links[links$date == issue_date,]$file_link
    
    data <- suppressMessages(readr::read_csv(link))
    if (geography[1] == "global"){
      data <- calc_jhu_global_cum(data)
    }
    
    jhu_data <- tibble::tibble(
      issue_date = list(as.character(issue_date)),
      data = list(data))
  }
  
  return(jhu_data)
}

#' Get all available truth data issue dates
#' 
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths', 'cases' or 'hospitalizations'
#' @param geography character, which data to read. Default is "US", other option is
#' "global"
#' 
#' @return date vector of all available issue_date
#' 
#' @export
available_issue_dates <- function(measure,  geography = c("US", "global")){
  if (measure == "hospitalizations"){
    #retrieve data update history
    healthdata_timeseries_history <- healthdata_timeseries_history()
    healthdata_dailyrevision_history <- healthdata_dailyrevision_history()
    
    all_avail_issue_date <- unique(c(
      healthdata_timeseries_history$issue_date,
      healthdata_dailyrevision_history$issue_date))
    all_avail_issue_date <- unique(c(
      all_avail_issue_date,
      covidData::healthdata_hosp_early_data$issue_date))

    return(all_avail_issue_date)
  } else if (measure == "deaths"){
    links <- get_time_series_data_link(measure, geography)
    return(links$date)
  } else if (measure == "cases"){
    links <- get_time_series_data_link(measure, geography)
    return(links$date)
  }
}
