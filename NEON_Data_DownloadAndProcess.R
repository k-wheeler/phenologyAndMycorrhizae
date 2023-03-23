downloadNEONdata <- function(dataName,NEON_ID,andStack=TRUE,includedSeq=seq(1:47)){
  #Create Site-Specific Folders for Downloading 
  for(s in seq_along(NEON_siteNames)){
    print(NEON_siteNames[s])
    dir.create(paste0(dataPath,dataName,'/',NEON_siteNames[s]))
  }
  #Download and Stack the Data
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  for(s in seq_along(NEON_siteNames)[includedSeq]){
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
      if(file.exists(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName))){
        subDat <- read.csv(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName))
        if(dataName%in%c("NEON_soilProperties","NEON_Roots","NEON_litterfall","NEON_plantFoliarTraits")){
          allData <- rbind(allData,subDat[,c('siteID','collectDate',selectColumns)])
        }else if(dataName=="NEON_vegetation_structure"){
          allData <- rbind(allData,subDat[,c('siteID','date',selectColumns)])
        }else{
          allData <- rbind(allData,subDat[,c('siteID','startDateTime',selectColumns)])
        }
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

calculateDailyWeather <- function(dataName,dataPath,varName,funName){
  if(funName=="mean"){
    funType <- mean
  }else if(funName=="min"){
    funType <- min
  }else if(funName=="max"){
    funType <- max
  }else if(funName=="sum"){
    funType <- sum
  }
  inFileName <- paste0(dataName,"ALLdata.csv")
  outFileName <- paste0(dataName,"Dailydata_",as.character(funName),".csv")
  print(outFileName)
  allData <- read.csv(file=paste0(dataPath,inFileName))
  if(dataName%in%c("NEON_SingleAirTemperature", "NEON_PAR","NEON_SoilTemp","NEON_relativeHumidity","NEON_Windspeed","NEON_SoilMoisture")){
    allDaily <- matrix(nrow=0,ncol=5)
  }else{
    allDaily <- matrix(nrow=0,ncol=4)
  }
  
  if("precipQF" %in% colnames(allData)){
    names(allData)[names(allData)=="precipQF"] <- 'finalQF'
  }else if(dataName=="NEON_PAR"){
    names(allData)[names(allData)=="PARFinalQF"] <- 'finalQF'
  }else if(dataName=="NEON_relativeHumidity"){
    names(allData)[names(allData)=="RHFinalQF"] <- 'finalQF'
  }else if(dataName=="NEON_Windspeed"){
    names(allData)[names(allData)=="windSpeedFinalQF"] <- 'finalQF'
  }else if(dataName=="NEON_SoilMoisture"){
    names(allData)[names(allData)=="VSWCFinalQF"] <- 'finalQF'
  }
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    allData$oldValue <- allData[,varName]
    if(dataName%in%c("NEON_SingleAirTemperature", "NEON_PAR","NEON_SoilTemp","NEON_relativeHumidity","NEON_Windspeed","NEON_SoilMoisture")){
      subDat <- allData %>% filter(siteID==siteName) %>% 
        filter(finalQF==0 | is.na(finalQF)) %>%
        mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
        group_by(date,verticalPosition) %>% mutate(value=funType(oldValue,na.rm=TRUE),n=sum(!is.na(oldValue)))
      subDat <- data.frame(subDat)
      subDat <-subDat[,which(names(subDat) %in% c('siteID','date','value','verticalPosition','n'))] %>% distinct()
    }else{
    subDat <- allData %>% filter(siteID==siteName) %>% 
      filter(finalQF==0 | is.na(finalQF)) %>%
      mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
      group_by(date) %>% mutate(value=funType(oldValue,na.rm=TRUE),n=n())
    subDat <- data.frame(subDat)
    subDat <-subDat[,which(names(subDat) %in% c('siteID','date','value','n'))] %>% distinct()
    }

    allDaily <- rbind(allDaily,subDat)
  }
  names(allDaily)[names(allDaily)=="value"] <- varName
  write.csv(file=paste0(dataPath,outFileName),allDaily,row.names = FALSE,quote=FALSE)
  
}

calculateGDD <- function(values,baseTemp=5){
  offsets <- values-baseTemp
  return(sum(offsets[offsets>0]))
}

calculateCDD <- function(values,baseTemp=10){
  offsets <- baseTemp-values
  return(sum(offsets[offsets>0]))
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

gapFillFromDayMet_verticalProfiles <- function(dataName,dataPath,varName,funName){
  allData <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character(funName),".csv"))
  dayMetLRs <- read.csv(paste0(dataName,"towerHeight_LRfitsWithDayMet_",as.character(funName),".csv"))
  dayMetLRs <- dayMetLRs %>% dplyr::select(siteID,week,verticalPosition,term,estimate)
  dayMetLRs <- pivot_wider(dayMetLRs,names_from = term,values_from = estimate)
  colnames(dayMetLRs) <- c("siteID","week","verticalPosition","intercept","slope")
  
  allOutput <- matrix(nrow=0,ncol=4)

  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    lat <- siteData$field_latitude[siteData$field_site_id==siteName]
    lon <- siteData$field_longitude[siteData$field_site_id==siteName]
    
    siteDat <- allData %>% filter(siteID==siteName)
    siteDat$date <- as.Date(siteDat$date)
    siteHeights <- unique(siteDat$verticalPosition)
    dayMet <- download_daymet(site=siteName,
                              lat=lat,
                              lon=lon,
                              start=2014,
                              end=2021,
                              internal=TRUE,
                              simplify=TRUE)
    dayMet$date <- as.Date((dayMet$yday-1),origin=as.Date(paste0(dayMet$year,"-01-01")))
    if(varName=='tempSingleMean'){
      dayMetMax <- dayMet %>% filter(measurement=="tmax..deg.c.")
      dayMetMax <- dayMetMax %>% dplyr::select(value,date)
      
      dayMetMin <- dayMet %>% filter(measurement=="tmin..deg.c.")
      dayMetMin <- dayMetMin %>% dplyr::select(value,date)
      dayMet <- merge(dayMetMax,dayMetMin,by="date")
      if(funName=="mean"){
        dayMet$dayMetValue <- rowMeans(dayMet[,2:3])
      }else if(funName=="min"){
        dayMet$dayMetValue <- dayMet[,3]
      }else if(funName=="max"){
        dayMet$dayMetValue <- dayMet[,2]
      }
      siteDat$NEON_value <- siteDat$tempSingleMean
      siteDat <- siteDat %>% dplyr::select(-tempSingleMean)
      dayMet <- dayMet %>% dplyr::select(-c(value.x,value.y))
    }
    allDayMet <- matrix(nrow=0,ncol=ncol(dayMet))
    for(h in seq_along(siteHeights)){
      newDayMet <- dayMet
      newDayMet$verticalPosition <- siteHeights[h]
      allDayMet <- rbind(allDayMet,newDayMet)
    }
    
    siteDatAll <- merge(allDayMet,siteDat,by=c('date','verticalPosition'),all=TRUE)
    siteDatAll$siteID <- siteName
    
    siteDatAll$week <- floor((lubridate::yday(siteDatAll$date)-0.01)/7)+1
    siteDatAll <- left_join(siteDatAll,dayMetLRs,by=c('week','verticalPosition','siteID'))

    siteDatAll$NEON_value[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] <- siteDatAll$slope[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] * 
      siteDatAll$dayMetValue[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] + siteDatAll$intercept[is.na(siteDatAll$NEON_value)|siteDatAll$n<20]
    siteDatAll <- siteDatAll[,c('siteID','date','verticalPosition','NEON_value')]
    colnames(siteDatAll)[colnames(siteDatAll)=="NEON_value"] <- paste(varName,funName,sep="_")

    allOutput <- rbind(allOutput,siteDatAll)
  }

  write.csv(file=paste0(dataPath,dataName,'Dailydata_',funName,'_gapFilled.csv'),allOutput,row.names = FALSE,quote=FALSE)
}

gapFillFromERA_verticalProfiles <- function(dataName,dataPath,varName,funName){
  allData <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character(funName),".csv"))
  ERALRs <- read.csv(file=paste0(dataName,"towerHeight_LRfitsWithERA5_",dataName,"_",funName,".csv"))
  
  ERAdat <- read.csv(paste0(dataPath,'ERA5_metData.csv'),header=TRUE)
  if(dataName=="NEON_SingleAirTemperature"){
    ERAdataName <- 't2m'
  }else if(dataName=="NEON_PrecipitationData"){ #Total precipitation
    ERAdataName <- "tp"
  }else if(dataName=="NEON_SoilTemp"){#Soil temperature level 1
    ERAdataName <- "stl1"
  }else if(dataName=="NEON_SoilMoisture"){
    ERAdataName <- "swvl1"
  }
  ERAdat <- ERAdat %>% filter(var==ERAdataName)
  if(ERAdataName%in%c("t2m","stl1")){
    ERAdat$value <- as.numeric(ERAdat$value)-273 #Convert K to C
  }
  
  ERAdat <- pivot_wider(ERAdat,names_from=2,values_from=3)
  
  allOutput <- matrix(nrow=0,ncol=4)
  
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    
    siteDat <- allData %>% filter(siteID==siteName)
    if(dataName=="NEON_PrecipitationData"){
      siteDat$verticalPosition <- "top"
    }
    
    siteDat$date <- as.Date(siteDat$date)
    siteHeights <- unique(siteDat$verticalPosition)
    
    ERAdatSite <- ERAdat %>% filter(siteID==siteName)
    
    if(funName=="mean"){
      ERAdatSite$ERAValue <- ERAdatSite$mean
    }else if(funName=="min"){
      ERAdatSite$ERAValue <- ERAdatSite$min
    }else if(funName=="max"){
      ERAdatSite$ERAValue <- ERAdatSite$max
    }else if(funName=="sum"){
      ERAdatSite$ERAValue <- ERAdatSite$sum
    }
    
    if(varName=='tempSingleMean'){
      siteDat$NEON_value <- siteDat$tempSingleMean
      siteDat <- siteDat %>% dplyr::select(-tempSingleMean)
    }else if(varName=='precipBulk'){
      siteDat$NEON_value <- siteDat$precipBulk
      siteDat <- siteDat %>% dplyr::select(-precipBulk)
    }else if(varName=='soilTempMean'){
      siteDat$NEON_value <- siteDat$soilTempMean
      siteDat <- siteDat %>% dplyr::select(-soilTempMean)
    }else if(varName=="VSWCMean"){
      siteDat$NEON_value <- siteDat$VSWCMean
      siteDat <- siteDat %>% dplyr::select(-VSWCMean)
    }
    ERAdatSite <- ERAdatSite %>% dplyr::select(date,siteID,ERAValue)
    allDayMet <- matrix(nrow=0,ncol=ncol(ERAdatSite)) ##
    if(length(siteHeights)==0 & varName %in% c("soilTempMean","VSWCMean")){
      newERAdatSite <- ERAdatSite
      newERAdatSite$verticalPosition <- 501
      allDayMet <- rbind(allDayMet,newERAdatSite)
    }
    for(h in seq_along(siteHeights)){
      newERAdatSite <- ERAdatSite
      newERAdatSite$verticalPosition <- siteHeights[h]
      allDayMet <- rbind(allDayMet,newERAdatSite)
    }
    if(nrow(allDayMet)>0){
      allDayMet$date <- as.Date(allDayMet$date)
      siteDat$date <- as.Date(siteDat$date)
      
      siteDatAll <- merge(allDayMet,siteDat,by=c('siteID','date','verticalPosition'),all=TRUE)
      
      siteDatAll$week <- floor((lubridate::yday(as.Date(as.character(siteDatAll$date)))-0.01)/7)+1
      siteDatAll <- left_join(siteDatAll,ERALRs,by=c('week','verticalPosition','siteID'))
      
      siteDatAll$NEON_value[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] <- siteDatAll$slope[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] * 
        siteDatAll$ERAValue[is.na(siteDatAll$NEON_value)|siteDatAll$n<20] + siteDatAll$intercept[is.na(siteDatAll$NEON_value)|siteDatAll$n<20]
    }else{
      siteDatAll <- allDayMet
      siteDatAll$NEON_value <- siteDatAll$ERAValue
      siteDatAll$ERAValue <- NULL
    }
    if(funName=="sum"){
      siteDatAll$NEON_value[siteDatAll$NEON_value<0] <- 0
    }
    
    siteDatAll <- siteDatAll[,c('siteID','date','verticalPosition','NEON_value')]
    colnames(siteDatAll)[colnames(siteDatAll)=="NEON_value"] <- paste(varName,funName,sep="_")
    
    allOutput <- rbind(allOutput,siteDatAll)
  }
  
  write.csv(file=paste0(dataPath,dataName,'Dailydata_',funName,'_ERAgapFilled.csv'),allOutput,row.names = FALSE,quote=FALSE)
}
