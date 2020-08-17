#' Replace negative incidence with imputed data and redistribute
#' residuals across observations before adjustment date
#'
#' @param data data frame
#' @param measure death or case
#' @return data frame after replacement
#'
#' @export
#'
replace_negatives <- function(data, measure) {

  # find observations with negative inc
  adjustments <- covidData::get_negative_cases(data)

  # replace negative incidents with imputed data for all replated locations
  for (i in 1:nrow(adjustments)) {
    case <- adjustments[i, ]
    seed <- 1234
    adjustment_location <- case$fips
    
    # get county, state and national locations
    location_data <- data %>%
      dplyr::filter(location == stringr::str_sub(adjustment_location, start = 1, end = 2) |
                      location == "US" | location == adjustment_location) 
    
    for (fips in unique(location_data$location)) {

      imputed_data <- covidData::adjust_daily_incidence(data[data$location == fips, ], case, seed, measure)

      data[data$location == fips, ]$inc <- imputed_data
    }
  }

  return(data)
}
