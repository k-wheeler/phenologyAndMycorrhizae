#Calculate Summary Metrics for Weather
source('sharedVariables.R')
#1 is fail data quality flag and 0 is pass
#Daily Temperature Mean
allTempData <- read.csv(file=paste0(dataPath,'allTemperatureData.csv'))
allDailyMeanTemp <- matrix(nrow=0,ncol=3)
for(s in seq_along(NEON_siteNames)){
  siteName <- NEON_siteNames[s]
  print(siteName)
  subDat <- allTempData %>% filter(siteID==siteName) %>% 
    filter(finalQF==0 | is.na(finalQF)) %>%
    mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
    group_by(date) %>% mutate(dailyMeanTemp=mean(tempTripleMean))
  subDat <- data.frame(subDat)
  subDat <-subDat[,which(names(subDat) %in% c('siteID','date','dailyMeanTemp'))] %>% distinct()
  allDailyMeanTemp <- rbind(allDailyMeanTemp,subDat)
}

#Daily Precipitation Total
allPrecipData <- read.csv(file=paste0(dataPath,'allPrecipData.csv'))
allDailyTotalPrecip <- matrix(nrow=0,ncol=3)
for(s in seq_along(NEON_siteNames)){
  siteName <- NEON_siteNames[s]
  print(siteName)
  subDat <- allPrecipData %>% filter(siteID==siteName) %>% 
    filter(precipQF==0 | is.na(precipQF)) %>%
    mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
    group_by(date) %>% mutate(dailyTotalPrecip=sum(precipBulk))
  subDat <- data.frame(subDat)
  subDat <-subDat[,which(names(subDat) %in% c('siteID','date','dailyTotalPrecip'))] %>% distinct()
  allDailyTotalPrecip <- rbind(allDailyTotalPrecip,subDat)
}

subDat <- allDailyTotalPrecip %>% filter(siteID=="HARV")
plot(subDat$date,subDat$dailyTotalPrecip,type="l")
