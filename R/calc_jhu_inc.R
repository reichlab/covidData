#' Assembles a data frame of incident deaths or cases from raw case or death data 
#' from JHU.
#'
#' @param jhu_data raw JHU data
#'
#' @return data frame with columns location (fips code), date, and inc
#' @export
#'
#' @examples

calc_jhu_inc <- function(jhu_data){
  jhu_data <- jhu_data %>% 
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
  county_results <- jhu_data %>%
    dplyr::filter(FIPS > 100) %>%
    dplyr::mutate(
      location = sprintf("%05d", FIPS)
    ) %>%
    dplyr::filter(location < "80001") %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(inc = diff(c(0, cum))) %>%
    dplyr::select(location, date, inc) %>%
    dplyr::ungroup()
    
  results <- dplyr::bind_rows(results, county_results)
  
  
  # summarized results for state level
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
    dplyr::select(location, date, inc)
  
  results <- dplyr::bind_rows(results, state_results)
  
  
  # summarized results for national level
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
    dplyr::select(location, date, inc)
  
  results <- dplyr::bind_rows(results, national_results)
  
  return(results)
  
}
