#' Preprocess JHU global data to get regional counts
#'
#' @param jhu_data raw JHU global data
#'
#' @return data frame with columns location and date columns
#' @export
#'
calc_jhu_global_cum <- function (jhu_data) {
  locations_to_aggregate <- c("Australia","Canada", "China")
  locations_total <- c("Denmark","France", "Netherlands", "United Kingdom")
  
  # only keep regional-level counts
  jhu_data <- jhu_data[!((jhu_data$`Country/Region` %in% locations_total) &
                           !is.na(jhu_data$`Province/State`)),]
    
  # aggregate sub-regional counts
  jhu_data <- jhu_data %>%
    dplyr::group_by(`Country/Region`) %>%
    dplyr:: summarise(dplyr::across(.cols = 4:(ncol(jhu_data) - 1), .fns = sum))
  
  return(jhu_data)
}
