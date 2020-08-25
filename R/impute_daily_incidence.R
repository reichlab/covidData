#' Imputed incidence value for the single day with reporting anomaly
#'
#' @param data time series data for one location related to an adjustment 
#' case. It has location, date, cum and inc as columns.
#' @param adjustment_date date from adjustment case
#' @param measure String specifying measure: 'cases' or 'deaths'
#' @return incidence columns after adjustments
#' @export
impute_daily_incidence <- function(data, adjustment_date, measure) {
  if (measure == 'deaths') {
    imputed_data <- covidData::jhu_deaths_imputed_data
  } else {
    imputed_data <- covidData::jhu_cases_imputed_data
  }

  location <- unique(data$location)
  date <- as.Date(adjustment_date)
  
  imputed_data[which(
    imputed_data$location == location &
    imputed_data$date == date
  ), ]$inc
}
