#' Assembles a data frame of incident deaths or cases from raw case or death data 
#' from JHU.
#'
#' @param jhu_data raw JHU data
#'
#' @return data frame with columns location (fips code), date, and inc
#' @export
#'
#' @examples
calc_jhu_inc <- function(jhu_data) {
  # select just columns with FIPS code, Province_State, and cumulative counts
  # each date
  jhu_data <- jhu_data %>%
    dplyr::select(FIPS, Province_State, dplyr::matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"))
  
  # calculate incident counts from cumulative counts
  jhu_data[, -(1:3)] <- t(diff(t(as.matrix(jhu_data[, -(1:2)]))))

  # reformat dates in column names to yyyy-mm-dd format
  colnames(jhu_data)[-(1:2)] <- colnames(jhu_data)[-(1:2)] %>%
    lubridate::mdy() %>%
    as.character()
  
  # reformat FIPS to 5 character string with leading zeros and
  # rename as location
  jhu_data <- jhu_data %>%
    dplyr::mutate(
      location = sprintf("%05d", FIPS)
    ) %>%
    dplyr::select(-FIPS)

  # pivot longer; each row will then contain observed incidence for a single
  # combination of location and date
  jhu_data <- jhu_data %>%
    tidyr::pivot_longer(
      -tidyselect::any_of(c("location", "Province_State")),
      names_to = "date",
      values_to = "inc"
    )

  return(jhu_data)
}
