
# replace -inc --> 0 and put it on the day before it
# call adjust_daily_inc with cases = dates w/ -inc
replace_negatives <- function (data){
  
  # find  observations with negative inc
  adjustments = get_negative_cases(data)
  
  # replace negative incidents
  for (i in 1:nrow(adjustments)){
    case = adjustments[i,]
    seed = 1234
    location = case$fips
    
    #data[data$location == location,]$inc = 0
    
    imputed_data = adjust_daily_incidence(data[data$location == location,],case,seed)
    
    data[data$location == location,]$inc = imputed_data
  }
  
  return (data)
}
