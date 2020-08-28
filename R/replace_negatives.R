#' Replace negative incidence with imputed data and redistribute
#' residuals across observations before adjustment date
#'
#' @param data data frame with location, date, cum and inc
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @return data frame with location, date, cum and inc after replacement
#'
#' @export
#'
replace_negatives <- function(data, measure) {

  # find observations with negative inc
  adjustments <- covidData::get_negative_cases(data)

  # replace negative incidence with imputed data for all replaced locations
  for (i in seq_len(nrow(adjustments))) {
    case <- adjustments[i, ]
    adjustment_location <- case$location
    adjustment_date <- case$date
    
    # get county, state and national locations
    location_data <- data %>%
      dplyr::filter(location == stringr::str_sub(adjustment_location, start = 1, end = 2) |
                      location == 'US' | location == adjustment_location) 
    
    for (fips in unique(location_data$location)) {
      imputed_data <- covidData::adjust_daily_incidence(
        data[data$location == fips, ], case$date, measure)
      
      data[data$location == fips, ]$inc <- imputed_data
    }
  }

  return(data)
}
