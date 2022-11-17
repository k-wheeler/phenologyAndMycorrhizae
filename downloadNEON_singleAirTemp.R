source('sharedVariables.R')

s=2
for(s in seq_along(NEON_siteNames)){
  print(NEON_siteNames[s])
  dir.create(paste0(dataPath,'NEON_SingleAirTemperature/',NEON_siteNames[s]))
}

zipsByProduct(dpID='DP4.00001.001',site="all",
              savepath=dataPath,check.size = F)

for(s in seq_along(NEON_siteNames)[37:47]){
  print(s)
  savePath <- paste0(dataPath,'NEON_SingleAirTemperature/',NEON_siteNames[s])
  zipsByProduct(dpID='DP1.00002.001',site=NEON_siteNames[s],
                savepath=savePath,check.size = F)
  stackByTable(paste0(savePath,'/filesToStack00002'))
}

allSingleAirTempData <- matrix(nrow=0,ncol=7)
colnames(allSingleAirTempData) <- c('siteID','startDateTime','horizontalPosition','verticalPosition',
                             'tempSingleMean','tempSingleExpUncert','finalQF')
for(s in seq_along(NEON_siteNames)[1:47]){
  print(NEON_siteNames[s])
    subDat <- read.csv(paste0(dataPath,"/NEON_SingleAirTemperature/",NEON_siteNames[s],'/filesToStack00002/stackedFiles/SAAT_30min.csv'))
    
  allSingleAirTempData <- rbind(allSingleAirTempData,subDat[,c('siteID','startDateTime','horizontalPosition','verticalPosition',
                                                               'tempSingleMean','tempSingleExpUncert','finalQF')])
  
}
write.csv(file=paste0(dataPath,'allSingleAirTempData.csv'),allSingleAirTempData,row.names = FALSE,quote=FALSE)

##Investigating Correlations
allTempData <- read.csv(paste0(dataPath,'allTemperatureData.csv'))
pdf('singleVsTripleTemp.pdf',height=3,width=6)
#par(mfrow=c(2,1))
for(s in seq_along(NEON_siteNames)[11:47]){
  siteName <- NEON_siteNames[s]
  print(siteName)
  lat <- siteData$field_latitude[siteData$field_site_id==siteName]
  lon <- siteData$field_longitude[siteData$field_site_id==siteName]
  subDat <- allSingleAirTempData %>% filter(siteID==siteName)
  subDat <- subDat %>% filter(verticalPosition==max(unique(subDat$verticalPosition)))
  
  subDat2 <- allTempData %>% filter(siteID==siteName)
  
  dayMet <- download_daymet(site=siteName,
                            lat=lat,
                            lon=lon,
                            start=lubridate::year(min(subDat$startDateTime)),
                            end=2021,
                            internal=TRUE,
                            simplify=TRUE)
  
  subDatAll <- left_join(subDat,subDat2,by='startDateTime')


  plot(as.Date(subDat$startDateTime),subDat$tempSingleMean,pch=20,main=siteName)
  points(as.Date(subDat2$startDateTime),subDat2$tempTripleMean,pch=20,col="red")

  plot(subDatAll$tempSingleMean,subDatAll$tempTripleMean,pch=20,main=siteName)
  sm <- summary(lm(tempSingleMean~tempTripleMean,data = subDatAll))
  text((min(subDatAll$tempSingleMean,na.rm=TRUE)+10),(max(subDatAll$tempTripleMean,na.rm=TRUE)-10),sm$r.squared)
}
dev.off()
