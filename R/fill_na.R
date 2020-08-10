#' Replace daily inc with NA in specific rows
#' 
#' @param results a result data.frame
#' @param adjustments a adjustment case data.frame 
#' @return a data.frame with MAs on adjustment cases
#' @export
fill_na <- function(results, adjustments){
  print('na')
  results = results %>%
    dplyr::rowwise() %>%
    dplyr:: mutate(
      # for each row, if date is in adjustment date and location is in adjustment states
      inc = ifelse(date %in% adjustments$dates[adjustments$fips==location] |
                     # if first two digits of county fips is the state fips of adjustment states
                     date %in% adjustments$dates[adjustments$fips==stringr::str_sub(location, start = 1, end=2)] ,NA_integer_,inc)) %>%
    dplyr::ungroup()
  return(results)
}
