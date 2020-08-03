get_negative_cases <- function(data){
  # find  observations with negative inc
  locations = c()
  dates = c()
  
  for (i in 1:nrow(data)){
    if (data[i,]$inc <0) {
      locations = c(locations, as.character(data[i,]$location))
      dates = c(dates, as.character(data[i,]$date))
    }
  }
  
  adjustments = data.frame(fips = locations,dates = dates)
  
return(adjustments)
}
