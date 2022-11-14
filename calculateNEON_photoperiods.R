source('sharedVariables.R')
library('suncalc')

dates <- seq(as.Date("2016-01-01"),as.Date("2021-12-31"),by="day")

allSunlightTimes <- matrix(nrow=(nrow(siteData)*length(dates)),ncol=7)
allSunlightTimes <- data.frame(allSunlightTimes)
colnames(allSunlightTimes) <- c("siteID",'date',"lat","long",'sunrise','sunset','daylength')
allSunlightTimes$date <- as.Date("2000-01-01")
allSunlightTimes$sunrise <- Sys.time()
allSunlightTimes$sunset <- Sys.time()
ct <- 1
for(s in 1:nrow(siteData)){
  print(siteData$field_site_id[s])
  lat <- siteData$field_latitude[s]
  long <- siteData$field_longitude[s]
  for(d in 1:length(dates)){
    if(d%%100==0){
      print(dates[d])
    }
    suntimes <- getSunlightTimes(date=dates[d],
                                 lat=lat,lon=long,keep=c("sunrise","sunset"),
                                 tz = "UTC")
    allSunlightTimes[ct,'siteID'] <- siteData$field_site_id[s]
    allSunlightTimes[ct,'date'] <- dates[d]
    allSunlightTimes[ct,'lat'] <- lat
    allSunlightTimes[ct,'long'] <- long
    allSunlightTimes[ct,'sunrise'] <- suntimes$sunrise
    allSunlightTimes[ct,'sunset'] <- suntimes$sunset
    allSunlightTimes[ct,'daylength'] <- as.numeric((suntimes$sunset-suntimes$sunrise))
    ct <- ct + 1
  }
}
write.csv(allSunlightTimes,file=paste0(dataPath,'NEON_sunlightTimes.csv'),row.names=FALSE,quote=FALSE)
