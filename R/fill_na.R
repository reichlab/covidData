#' Replace daily inc with NA in specific rows
#'
#' @param results a result data.frame. It has location,
#' date, cum and inc as columns.
#' @param adjustments a adjustment case data.frame
#' @return a data.frame with NAs on adjustment cases
#' @export
fill_na <- function(results, adjustments) {
  adjustments$date <- as.character(adjustments$date)

  results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # for each row, if date is in adjustment date and location is in adjustment states
      inc = ifelse(date %in% adjustments$date[adjustments$location == location] |
        # if first two digits of county fips is the state fips of adjustment states
        date %in% adjustments$date[adjustments$location == stringr::str_sub(location, start = 1, end = 2)], NA_integer_, inc)
    ) %>%
    dplyr::ungroup()
}
