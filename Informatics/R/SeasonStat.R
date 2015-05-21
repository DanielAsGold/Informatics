
SeasonStat = function(SeasonStart,SeasonEnd, clim){ 

  spring=subset(clim, month >= (SeasonStart & month <= SeasonEnd),list(year=clim$year))
  MeanMaxTemp=mean(spring$tmax) 
  MeanMinTemp=mean(spring$tmin) 
  MinTempYr=spring[which(spring[,2]==min(spring[,2])), 1] 
  MaxPrecipYr=spring[which(spring[,4]==max(spring[,4])), 1] 
  MeanSpringRain=mean(spring$rain)#calculate mean spring rain
  result=data.frame("Mean_Spring_Max"=MeanMaxTemp,"Mean_Spring_Min"=MeanMinTemp,"Coldest_Spring"=MinTempYr,"Wettest_Year"=MaxPrecipYr,"Average_Spring_Precipitation"=MeanSpringRain)
  return(result) 
}
