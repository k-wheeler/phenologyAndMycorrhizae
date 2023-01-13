downloadNEONdata <- function(dataName,NEON_ID,andStack=TRUE){
  #Create Site-Specific Folders for Downloading 
  for(s in seq_along(NEON_siteNames)){
    print(NEON_siteNames[s])
    dir.create(paste0(dataPath,dataName,'/',NEON_siteNames[s]))
  }
  #Download and Stack the Data
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  for(s in seq_along(NEON_siteNames)[3:47]){
    print(s)
    savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
    zipsByProduct(dpID=NEON_ID,site=NEON_siteNames[s],
                  savepath=savePath,check.size = F)
    if(andStack){
      stackByTable(paste0(savePath,'/filesToStack',IDnum))
    }
  }
}

combineNEONdata <- function(dataName,NEON_ID,selectColumns,inFileName,dataPath,saveFile=TRUE){
  allData <- matrix(nrow=0,ncol=(length(selectColumns)+2))
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  if(length(inFileName)==1){
    for(s in seq_along(NEON_siteNames)){
      print(NEON_siteNames[s])
      subDat <- read.csv(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName))
      if(dataName=="NEON_soilProperties"){
        allData <- rbind(allData,subDat[,c('siteID','collectDate',selectColumns)])
      }else{
        allData <- rbind(allData,subDat[,c('siteID','startDateTime',selectColumns)])
      }
    }
  }else if(length(inFileName)==2){
    for(s in seq_along(NEON_siteNames)){
      print(NEON_siteNames[s])
      potentialFile1 <- paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName[1])
      if(file.exists(potentialFile1)){
        subDat <- read.csv(potentialFile1)
        subDat <- subDat %>% mutate(precipBulk=priPrecipBulk,precipExpUncert=priPrecipExpUncert,precipQF=priPrecipFinalQF)
      }else{
        subDat <- read.csv(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName[2]))
        subDat <- subDat %>% mutate(precipBulk=secPrecipBulk,precipExpUncert=secPrecipExpUncert,precipQF=secPrecipSciRvwQF)
      }
      allData <- rbind(allData,subDat[,c('siteID','startDateTime',selectColumns)])
    }
  }
  outFileName <- paste0(dataName,"ALLdata.csv")
  if(saveFile){
    write.csv(file=paste0(dataPath,outFileName),allData,row.names = FALSE,quote=FALSE)
  }else{
    return(allData)
  }
}

calculateDailyWeather <- function(dataName,dataPath,varName,funType){
  inFileName <- paste0(dataName,"ALLdata.csv")
  outFileName <- paste0(dataName,"Dailydata.csv")
  allData <- read.csv(file=paste0(dataPath,inFileName))
  allDaily <- matrix(nrow=0,ncol=3)
  if("precipQF" %in% colnames(allData)){
    names(allData)[names(allData)=="precipQF"] <- 'finalQF'
  }
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    allData$oldValue <- allData[,varName]
    subDat <- allData %>% filter(siteID==siteName) %>% 
      filter(finalQF==0 | is.na(finalQF)) %>%
      mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
      group_by(date) %>% mutate(value=funType(oldValue,na.rm=TRUE))
    subDat <- data.frame(subDat)
    subDat <-subDat[,which(names(subDat) %in% c('siteID','date','value'))] %>% distinct()
    allDaily <- rbind(allDaily,subDat)
  }
  names(allDaily)[names(allDaily)=="value"] <- varName
  write.csv(file=paste0(dataPath,outFileName),allDaily,row.names = FALSE,quote=FALSE)
  
}

gapFillFromDaymet <- function(dataName,dataPath,varName){
  allData <- read.csv(paste0(dataPath,dataName,'Dailydata.csv'))
  allOutput <- matrix(nrow=0,ncol=3)
  # pdf('dayMetVsTripleTemp.pdf',height=9,width=9)
  # par(mfrow=c(3,3))
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    lat <- siteData$field_latitude[siteData$field_site_id==siteName]
    lon <- siteData$field_longitude[siteData$field_site_id==siteName]
    
    subDat <- allData %>% filter(siteID==siteName)
    subDat$date <- as.Date(subDat$date)
    
    dayMet <- download_daymet(site=siteName,
                              lat=lat,
                              lon=lon,
                              start=2014,
                              end=2021,
                              internal=TRUE,
                              simplify=TRUE)
    dayMet$date <- as.Date((dayMet$yday-1),origin=as.Date(paste0(dayMet$year,"-01-01")))
    if(varName=='tempTripleMean'){
      subDat$NEON_value <- subDat$tempTripleMean
      dayMetMax <- dayMet %>% filter(measurement=="tmax..deg.c.")
      dayMetMax <- dayMetMax %>% dplyr::select(value,date)
      
      dayMetMin <- dayMet %>% filter(measurement=="tmin..deg.c.")
      dayMetMin <- dayMetMin %>% dplyr::select(value,date)
      dayMet <- merge(dayMetMax,dayMetMin,by="date")
      dayMet$gapFillValue <- rowMeans(dayMet[,2:3])
    }else if(varName=="precipBulk"){
      subDat$NEON_value <- subDat$precipBulk
      dayMet <- dayMet %>% filter(measurement=="prcp..mm.day.")
      dayMet$gapFillValue <- dayMet$value
    }
    subDatAll <- merge(dayMet,subDat,by='date',all=TRUE)
    subDatAll$siteID <- siteName
    mdl <- lm(NEON_value~gapFillValue,data = subDatAll)
    # sm <- summary(mdl)
    # plot(subDatAll$dayMetTemp,subDatAll$tempTripleMean,pch=20,main=siteName,
    #      ylab="NEON Top of Tower Temperature",xlab="Daymet Temperature")
    # abline(1,1,col="red",lwd=3)
    # abline(lm(tempTripleMean~dayMetTemp,data = subDatAll),col="cyan",lwd=3)
    # text((min(subDatAll$dayMetTemp,na.rm=TRUE)+10),(max(subDatAll$tempTripleMean,na.rm=TRUE)-10),round(sm$r.squared,digits=3))
    # 
    subDatAll$NEON_value[is.na(subDatAll$NEON_value)] <- predict(mdl,subDatAll[is.na(subDatAll$NEON_value),])
    subDatAll <- subDatAll[,c('date','siteID','NEON_value')]
    colnames(subDatAll)[colnames(subDatAll)=="NEON_value"] <- varName
    
    #plot(subDatAll$date,subDatAll$NEON_value,pch=20)
    allOutput <- rbind(allOutput,subDatAll)
  }
  # dev.off()
  write.csv(file=paste0(dataPath,dataName,'Dailydata_gapFilled.csv'),allOutput,row.names = FALSE,quote=FALSE)
}
