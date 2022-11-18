#Calculate Summary Metrics for Weather
source('sharedVariables.R')
#1 is fail data quality flag and 0 is pass
#Daily Triple Temperature Mean ----
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

#Daily Precipitation Total ----
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

#Daily Air Temperature Mean For Different Levels ----
allSingleAirTempData <- read.csv(file=paste0(dataPath,'allSingleAirTempData.csv'))
allDailyMeanTemp <- matrix(nrow=0,ncol=4)
for(s in seq_along(NEON_siteNames)){
  siteName <- NEON_siteNames[s]
  print(siteName)
  subDat <- allSingleAirTempData %>% filter(siteID==siteName) %>% 
    filter(finalQF==0 | is.na(finalQF))
  
  verticalHeights <- unique(subDat$verticalPosition)
  print(verticalHeights)
  # for(h in seq_along(verticalHeights)){
  #   subSubDat <- subDat %>% filter(verticalPosition==verticalHeights[h]) %>%
  #     mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
  #     group_by(date) %>% mutate(dailyMeanTemp=mean(tempSingleMean))
  #   subSubDat <- data.frame(subSubDat)
  #   subSubDat <-subSubDat[,which(names(subSubDat) %in% c('siteID','date','dailyMeanTemp'))] %>% distinct()
  #   subSubDat$verticalPosition <- verticalHeights[h]
  #   allDailyMeanTemp <- rbind(allDailyMeanTemp,subSubDat)
  # }
}

subDat <- allDailyMeanTemp %>% filter(siteID=="HARV") %>% filter(verticalPosition==10)
plot(subDat$date,subDat$dailyMeanTemp,pch=20)
subDat <- allDailyMeanTemp %>% filter(siteID=="HARV") %>% filter(verticalPosition==20)
points(subDat$date,subDat$dailyMeanTemp,pch=20,col="cyan")
subDat <- allDailyMeanTemp %>% filter(siteID=="HARV") %>% filter(verticalPosition==30)
points(subDat$date,subDat$dailyMeanTemp,pch=20,col="blue")
subDat <- allDailyMeanTemp %>% filter(siteID=="HARV") %>% filter(verticalPosition==40)
points(subDat$date,subDat$dailyMeanTemp,pch=20,col="green")
subDat <- allDailyMeanTemp %>% filter(siteID=="HARV") %>% filter(verticalPosition==50)
points(subDat$date,subDat$dailyMeanTemp,pch=20,col="red")

#Calculate Coefficient of Determination from 1:1 line instead of line of best fit ----
calR2 <- function(obys,prys){
  prys <- prys[!is.na(obys)]
  obys <- obys[!is.na(obys)]
  prys <- prys[!is.na(prys)]
  obys <- obys[!is.na(prys)]
  
  obs.mean <- mean(obys)
  SStot <- sum((obys-obs.mean)**2)
  SSreg <- sum((prys-obs.mean)**2)
  SSres <- sum((obys-prys)**2)
  r2 <- (1-(SSres/SStot))
  if(r2<0){
    r2 <- 0
  }
  #adjr2 <- 1-(1-r2)*((length(obys)-1)/(length(obys)-2))
  return(r2)
}

tempComparedDayMet <- allDailyMeanTemp[,c('siteID','verticalPosition')] %>% distinct()
tempComparedDayMet$r2 <- NA
tempComparedDayMet$r2_1to1 <- NA
ct <- 1
for(s in seq_along(NEON_siteNames)){
  siteName <- NEON_siteNames[s]
  siteDat <- allDailyMeanTemp %>% filter(siteID==siteName)
  
  lat <- siteData$field_latitude[siteData$field_site_id==siteName]
  lon <- siteData$field_longitude[siteData$field_site_id==siteName]
  dayMet <- download_daymet(site=siteName,
                            lat=lat,
                            lon=lon,
                            start=lubridate::year(min(subDat$date)),
                            end=2021,
                            internal=TRUE,
                            simplify=TRUE)
  dayMetMax <- dayMet %>% filter(measurement=="tmax..deg.c.")
  dayMetMax$date <- as.Date((dayMetMax$yday-1),origin=as.Date(paste0(dayMetMax$year,"-01-01")))
  dayMetMax <- dayMetMax %>% dplyr::select(value,date)
  
  dayMetMin <- dayMet %>% filter(measurement=="tmin..deg.c.")
  dayMetMin$date <- as.Date((dayMetMin$yday-1),origin=as.Date(paste0(dayMetMin$year,"-01-01")))
  dayMetMin <- dayMetMin %>% dplyr::select(value,date)
  dayMet <- merge(dayMetMax,dayMetMin,by="date")
  dayMet$dayMetTemp <- rowMeans(dayMet[,2:3])
  
  for(i in seq_along(unique(siteDat$verticalPosition))){
    subDat <- siteDat %>% filter(verticalPosition==unique(siteDat$verticalPosition)[i])
    subDat <- left_join(subDat,dayMet,by="date")
    plot(subDat$dailyMeanTemp,subDat$dayMetTemp,pch=20,main=unique(siteDat$verticalPosition)[i])
    abline(0,1,col="red")
    # sm <- summary(lm(dayMetTemp~dailyMeanTemp,data=subDat))
    # tempComparedDayMet$r2[ct] <- sm$r.squared
    # tempComparedDayMet$r2_1to1[ct] <- calR2(obys=subDat$dayMetTemp,prys=subDat$dailyMeanTemp)
    # ct <- ct+1
  }
}


