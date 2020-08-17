#' Imputed incidence value for the single day with reporting anomaly
#'
#' @param data time series data for one location in adjustment case
#' @param adjustment_case a case to adjust
#' @param measure case or death
#' @return incidence columns after adjustments
#' @export

impute_daily_incidence <- function(data, adjustment_case, measure) {
  if (measure == "deaths") {
    imputed_data <- covidData::jhu_deaths_imputed_data
  } else {
    imputed_data <- covidData::jhu_cases_imputed_data
  }

  location <- unique(data$location)
  date <- as.Date(adjustment_case$date)
  
  unique(imputed_data[which(
    imputed_data$location == location &
    imputed_data$date == date
  ), ]$inc)
}
