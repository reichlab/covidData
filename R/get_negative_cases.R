#' Find all observations with negative incidence in a
#' given data frame
#'
#' @param data a data frame with location, date, cum and inc
#' @return a data frame of cases with locations and dates
#'
#' @export
#'
get_negative_cases <- function(data) {
  data %>%
    dplyr::filter(inc < 0) %>%
    dplyr::select(
      location = location,
      date = date
    )
}
