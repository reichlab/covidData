#' Find all observations with negative incidence in a
#' given data frame
#'
#' @param data data frame
#' @return a data frame of cases with locations and dates
#'
#' @export
#'
get_negative_cases <- function(data) {
  locations <- c()
  dates <- c()

  # Find  observations with negative inc
  for (i in 1:nrow(data)) {
    if (data[i, ]$inc < 0) {
      locations <- c(locations, as.character(data[i, ]$location))
      dates <- c(dates, as.character(data[i, ]$date))
    }
  }

  data.frame(fips = locations, dates = as.Date(dates))
}
