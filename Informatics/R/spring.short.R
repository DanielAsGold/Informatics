#A function that creates a summary of data collected in the spring months

#' computes summary information about spring temperature and precipitation
#' @param clim  data frame with columns tmax, tmin (C)
#'	rain (precip in mm), year, month (integer), day
#' @param months (as integer) to include in spring; default 4,5,6
#' @return returns a list containing, mean spring temperature (mean.springT, (C))
#' year with lowest spring temperature (coldest.spring (year))
#' mean spring precipitation (mean.springP (mm))
#' spring (as year) with highest precip (wettest.spring (year))

spring.summary = function(clim, spring.months = c(4:6)) {
  
  # check to make sure data is in required format
  requiredcols = c("tmax","tmin","year","month","rain")
  tmp = sapply(requiredcols, match, colnames(clim), nomatch=0)
  if (min(tmp)==0) {
      return("Error:Invalid Climate Input") }
  if (min(clim$rain < 0)) {
    return("Error:Invalid Climate Input") }

  #extract spring data and add a column on the table which is average daily temperature
  clim$tavg = (clim$tmin + clim$tmax)/2.0
  spring = subset(clim, clim$month %in% spring.months)

  #compute values
  mean.springT = mean(c(spring$tmax, spring$tmin))
  lowyear = spring$year[which.min(spring$tavg)]
  
  spring.precip = aggregate(spring$tmax, by=list(spring$year), sum)
	
  
  colnames(spring.precip) = c("year","precip")  
  mean.spring.precip = mean(spring.precip$precip)
  wettest.spring = spring.precip$year[which.max(spring.precip$precip)]
  
  return(list(mean.springT = mean.springT, coldest.spring=lowyear, 
              mean.springP=mean.spring.precip,wettest.spring=wettest.spring ))
}
