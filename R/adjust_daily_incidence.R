#' Redistribute difference of original observation and the 
#' imputed incidence value for the single day with reporting anomaly
#' 
#' @param data time series data for location in adjustment case
#' @param adjustment_case a case to adjust
#' @param measure case or death
#' @return incidence columns after redistribution
#' @export


# might need to take out all as.Date
adjust_daily_incidence <- function (data, adjustment_case,seed, measure){
  
  set.seed(seed)
  

  obs = data[which(data$date == as.Date(adjustment_case$date) &
                     # counties in adjustment state or US
                     (stringr::str_sub(data$ location, start = 1, end=2) %in% adjustment_case$fips |
                        data$location == 'US')),]$inc

  # read data
  replacement = round(impute_daily_incidence (data, adjustment_case, measure),digits=0)
  # if rep > obs  repl= obs for stan 
  
  if (obs > 0){
    replacement = ifelse(replacement > obs, obs, replacement)
  }
  
  
  diff = obs - replacement
  
  # replace observation with replacement value
  data = data %>% 
    dplyr::mutate(inc=replace(inc, (date == as.Date(adjustment_case$date) &
                                      stringr::str_sub(location, start = 1, end=2) %in%
                                      adjustment_case$fips),replacement))
  
  
  #in place
  data = data %>%
    dplyr:: mutate(new_cum = ifelse(date <= as.Date(adjustment_case$date),
                                       cumsum(inc),0)) %>%
    
    dplyr:: mutate(proportion = ifelse(date <= as.Date(adjustment_case$date),
                                       inc/new_cum[date == as.Date(adjustment_case$date)],0)) %>%
    dplyr:: rowwise() %>%
    dplyr:: mutate(inc = ifelse(date <= as.Date(adjustment_case$date),max(0,round(inc + diff * proportion,digits=0)),inc)) %>%
    dplyr:: ungroup() %>%
    dplyr:: select(location,date,cum,inc)
  
  
  # new_cum at adjustment_date 
  new_cum = sum(data[which(data$date <=as.Date(adjustment_case$date)),]$inc)

  diff = data[which(data$date == as.Date(adjustment_case$date)),]$cum - new_cum
  
  # vector of indices sort data by inc 
  sorted_inds = order(data[which(data$date<=as.Date(adjustment_case$date)),]$inc,decreasing = TRUE)
  
  # while diff != 0
  # inc >=1 add/- 1 to inc, else --> go to the beginning of inds
  # update diff
  
  inds_i = 1
  while(diff !=0){
    if (data[sorted_inds[inds_i],]$inc >= 1){
      if (diff > 0){
        # update inc
        data[sorted_inds[inds_i],]$inc = data[sorted_inds[inds_i],]$inc +1
        # update diff
        diff = diff -1
      } else{
        data[sorted_inds[inds_i],]$inc = data[sorted_inds[inds_i],]$inc -1
        diff = diff + 1
      }
      # update inds_i 
      inds_i = inds_i +1
    } else{
      # go back to the beginning of inds
      inds_i = 1
    }
  }
    
  #only return inc column
  return (data$inc)
}
