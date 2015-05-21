#creating a function called spring.smmary that summarizeds spring month data with the default interval of "month"
spring.summary = function(clim, spring.months = c(4:6), Interval="m") {
    
  #extract spring data and add a column on the table which is average daily temperature
  clim$tavg = (clim$tmin + clim$tmax)/2.0
  spring = subset(clim, clim$month %in% spring.months)

  #compute values, aggregate data so as to display the wettest year (most precip) and the coldest spring (lowest tavg)
  AgTemp=aggregate(spring$tavg, by=list(spring$year), mean) #aggregates the avg. temp data into average by year
  lowyear = AgTemp$Group.1[which.min(AgTemp$x)] #identifies the year with the coldest spring (average daily temp across 3 months)
  spring.precip = aggregate(spring$rain, by=list(spring$year), sum)
  WettestSpring = spring.precip$Group.1[which.max(spring.precip$x)]
  
  if(Interval=="y") {
    ColdYear=subset(spring,spring$year==lowyear, select=c(year,tavg))#subset "spring" so that the "year" is equal to the year with lowest spring temp
    Coldyear=aggregate(ColdYear$tavg,by=list(ColdYear$year),mean)
    RainYear=subset(spring,spring$year==WettestSpring, select=c(year,rain))
    RainYear=aggregate(RainYear$rain,by=list(RainYear$year),sum)
  }
  
  if(Interval=="m"){
    ColdYear=subset(spring,spring$year==lowyear, select=c(year,month,tavg))#subset "spring" so that the "year" is equal to the year with lowest spring temp
    ColdYear=aggregate(ColdYear$tavg,by=list(ColdYear$month, ColdYear$year),mean)
    RainYear=subset(spring,spring$year==WettestSpring, select=c(year,month,rain))
    RainYear=aggregate(RainYear$rain,by=list(RainYear$month,RainYear$year),sum)
}

if(Interval=="d"){
  ColdYear=subset(spring,spring$year==lowyear, select=c(year,month,day,tavg))#subset "spring" so that the "year" is equal to the year with lowest spring temp
  RainYear=subset(spring,spring$year==WettestSpring, select=c(year,month,day,rain))
}
return(list(ColdYear,RainYear))
}