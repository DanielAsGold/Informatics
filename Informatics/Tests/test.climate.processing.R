test_that("SeasonStat.works" ,
{
  clim.data= as.data.frame(cbind(month=c(1:4), day=rep(1, times=4), year=c(1:4), 
                                 rain=rep(0, times=4), tmax=c(2,2,1,1), tmin=rep(0, times=4)))
 
 expect_that(SeasonStat(1,4,clim.data)$MeanSpringRain, equals(0))
 expect_that(SeasonStat((1,4,clim.data))$MeanMinTemp, equals(0.5))
 expect_that(SeasonStat((1,4,clim.data)$MeanMaxTemp, equals(1))
 expect_that(SeasonStat(1,4,clim.data)$MinTempYr > 2, is_true())
})