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
  mean.springT = mean(spring$tavg) #computes the mean daily average temp in spring months
  mean.springT=round(mean.springT,2) #Rounds my answer to two decimal places
  AgTemp=aggregate(spring$tavg, by=list(spring$year), mean) #aggregates the avg. temp data into average by year
  lowyear = AgTemp$Group.1[which.min(AgTemp$x)] #identifies the year with the coldest spring (average daily temp across 3 months)
  
  spring.precip = aggregate(spring$rain, by=list(spring$year), sum)
	
  
  colnames(spring.precip) = c("Year","Rain")  
  MeanSpringRain = mean(spring.precip$Rain)
  MeanSpringRain=round(MeanSpringRain,2) #Rounds my answer to two decimal places
  WettestSpring = spring.precip$Year[which.max(spring.precip$Rain)]
  WettestSpring=round(WettestSpring,2) #Rounds my answer to two decimal places
  
  return(list("Mean Spring Temp" = mean.springT, "Coldest Spring"=lowyear, 
              "Mean Spring Rain"=MeanSpringRain, "Wettest Spring"=WettestSpring ))
}
