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
  AgTemp=aggregate(spring$tavg, by=list(spring$year), mean) #aggregates the avg. temp data into average by year
  lowyear = AgTemp$Group.1[which.min(AgTemp$x)] #identifies the year with the coldest spring (average daily temp across 3 months)
  
  spring.precip = aggregate(spring$rain, by=list(spring$year), sum)
  WettestSpring = spring.precip$Year[which.max(spring.precip$Rain)]
  
  return(list("Mean Spring Temp" = mean.springT, "Coldest Spring"=lowyear, 
              "Mean Spring Rain"=MeanSpringRain, "Wettest Spring"=WettestSpring ))
}
