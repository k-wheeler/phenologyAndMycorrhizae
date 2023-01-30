source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

dataName="NEON_SingleAirTemperature"

for(summaryStat in c("mean","min","max")){
  allData <- read.csv(paste0(dataPath,dataName,'Dailydata_',summaryStat,'.csv'))
  
  allData$week <- floor((lubridate::yday(allData$date)-0.01)/7)+1
  
  allFittedModelSummaries <- matrix(nrow=0,ncol=9)
  
  for(s in (seq_along(NEON_siteNames))){
    siteName <- NEON_siteNames[s]
    print(siteName)
    lat <- siteData$field_latitude[siteData$field_site_id==siteName]
    lon <- siteData$field_longitude[siteData$field_site_id==siteName]
    
    siteDat <- allData %>% filter(siteID==siteName)
    
    dayMet <- download_daymet(site=siteName,
                              lat=lat,
                              lon=lon,
                              start=2014,
                              end=2021,
                              internal=TRUE,
                              simplify=TRUE)
    
    
    dayMet$date <- as.Date((dayMet$yday-1),origin=as.Date(paste0(dayMet$year,"-01-01")))
    dayMetMax <- dayMet %>% filter(measurement=="tmax..deg.c.")
    dayMetMax <- dayMetMax %>% dplyr::select(value,date)
    
    dayMetMin <- dayMet %>% filter(measurement=="tmin..deg.c.")
    dayMetMin <- dayMetMin %>% dplyr::select(value,date)
    dayMet <- merge(dayMetMax,dayMetMin,by="date")
    if(summaryStat=="mean"){
      dayMet$dayMetValue <- rowMeans(dayMet[,2:3])
    }else if(summaryStat=="min"){
      dayMet$dayMetValue <- dayMet[,3]
    }else if(summaryStat=="max"){
      dayMet$dayMetValue <- dayMet[,2]
    }
    dayMet <- dayMet %>% dplyr::select(date,dayMetValue)
    
    siteHeights <- unique(siteDat$verticalPosition)
    siteDat$date <- as.Date(siteDat$date)
    siteDat <- left_join(siteDat,dayMet,by="date")
    
    fittedModels <- siteDat %>% group_by(week,verticalPosition) %>% 
      do(model = tryCatch(lm(tempSingleMean~dayMetValue,data=.),error=function(e){NA}))
    
    fittedModels <- subset(fittedModels,!is.na(model))
    
    fittedModelsTidied <- rowwise(fittedModels) %>% summarise(broom::tidy(model))
    fittedModelsFit <- rowwise(fittedModels) %>% summarise(glance(model))
    fittedModels <- cbind(fittedModels,fittedModelsFit$r.squared)
    
    newDat <- matrix(nrow=nrow(fittedModelsTidied),ncol=4)
    
    ct <- 1
    for(i in 1:nrow(fittedModels)){
      if(i%%100==0){
        print(i)
      }
      newDat[ct,1] <- siteName
      newDat[ct,2] <- as.character(fittedModels[i,1])
      newDat[ct,3] <- as.character(fittedModels[i,2])
      newDat[ct,4] <- as.character(fittedModels[i,4])
      ct <- ct + 1
      newDat[ct,1] <- siteName
      newDat[ct,2] <- as.character(fittedModels[i,1])
      newDat[ct,3] <- as.character(fittedModels[i,2])
      newDat[ct,4] <- as.character(fittedModels[i,4])
      ct <- ct + 1
    }
    
    colnames(newDat) <- c('siteID','week','verticalPosition','r.squared')
    fittedModelSummary <- cbind(newDat,fittedModelsTidied)
    allFittedModelSummaries <- rbind(allFittedModelSummaries,fittedModelSummary)
  }
  write.csv(allFittedModelSummaries,file=paste0(dataName,"towerHeight_LRfitsWithDayMet_",summaryStat,".csv"),quote=FALSE,row.names = FALSE)
  
}



# source('sharedVariables.R')
# source('NEON_Data_DownloadAndProcess.R')
# 
# dataName="NEON_SingleAirTemperature"
# 
# inFileName <- paste0(dataName,"ALLdata.csv")
# allData <- read.csv(file=paste0(dataPath,inFileName))
# 
# allData$week <- floor((lubridate::yday(allData$startDateTime)-0.01)/7)+1
# allData$hour <- lubridate::hour(allData$startDateTime)
# 
# allData <- allData %>%
#   mutate(verticalPosition=paste0("vertical_",verticalPosition)) %>% 
#   dplyr::select(-c(horizontalPosition,tempSingleExpUncert,finalQF))
# 
# allFittedModelSummaries <- matrix(nrow=0,ncol=10)
# 
# for(s in (seq_along(NEON_siteNames))){
#   print(NEON_siteNames[s])
#   siteDat <- allData %>% filter(siteID==NEON_siteNames[s])
#   siteHeights <- unique(siteDat$verticalPosition)
#   print(siteHeights)
#   siteDat <- pivot_wider(siteDat,names_from = verticalPosition,values_from = tempSingleMean)
#   
#   if(siteHeights[length(siteHeights)]=="vertical_30"){
#     siteDatModels <- siteDat %>% group_by(siteID,week,hour) %>% 
#       do(model_10_20 = tryCatch(lm(vertical_10 ~ vertical_20,data=.),error=function(e){NA}),
#          model_10_30 = tryCatch(lm(vertical_10 ~ vertical_30,data=.),error=function(e){NA}),
#          model_20_30 = tryCatch(lm(vertical_20 ~ vertical_30,data=.),error=function(e){NA}))
#   }else if(siteHeights[length(siteHeights)]=="vertical_40"){
#     siteDatModels <- siteDat %>% group_by(siteID,week,hour) %>% 
#       do(model_10_20 = tryCatch(lm(vertical_10 ~ vertical_20,data=.),error=function(e){NA}),
#          model_10_30 = tryCatch(lm(vertical_10 ~ vertical_30,data=.),error=function(e){NA}),
#          model_10_40 = tryCatch(lm(vertical_10 ~ vertical_40,data=.),error=function(e){NA}),
#          model_20_30 = tryCatch(lm(vertical_20 ~ vertical_30,data=.),error=function(e){NA}),
#          model_20_40 = tryCatch(lm(vertical_20 ~ vertical_40,data=.),error=function(e){NA}),
#          model_30_40 = tryCatch(lm(vertical_30 ~ vertical_40,data=.),error=function(e){NA}))
#   }else if(siteHeights[length(siteHeights)]=="vertical_50"){
#     siteDatModels <- siteDat %>% group_by(siteID,week,hour) %>% 
#       do(model_10_20 = tryCatch(lm(vertical_10 ~ vertical_20,data=.),error=function(e){NA}),
#          model_10_30 = tryCatch(lm(vertical_10 ~ vertical_30,data=.),error=function(e){NA}),
#          model_10_40 = tryCatch(lm(vertical_10 ~ vertical_40,data=.),error=function(e){NA}),
#          model_10_50 = tryCatch(lm(vertical_10 ~ vertical_50,data=.),error=function(e){NA}),
#          model_20_30 = tryCatch(lm(vertical_20 ~ vertical_30,data=.),error=function(e){NA}),
#          model_20_40 = tryCatch(lm(vertical_20 ~ vertical_40,data=.),error=function(e){NA}),
#          model_20_50 = tryCatch(lm(vertical_20 ~ vertical_50,data=.),error=function(e){NA}),
#          model_30_40 = tryCatch(lm(vertical_30 ~ vertical_40,data=.),error=function(e){NA}),
#          model_30_50 = tryCatch(lm(vertical_30 ~ vertical_50,data=.),error=function(e){NA}),
#          model_40_50 = tryCatch(lm(vertical_40 ~ vertical_50,data=.),error=function(e){NA}))
#   }else if(siteHeights[length(siteHeights)]=="vertical_60"){
#     siteDatModels <- siteDat %>% group_by(siteID,week,hour) %>% 
#       do(model_10_20 = tryCatch(lm(vertical_10 ~ vertical_20,data=.),error=function(e){NA}),
#          model_10_30 = tryCatch(lm(vertical_10 ~ vertical_30,data=.),error=function(e){NA}),
#          model_10_40 = tryCatch(lm(vertical_10 ~ vertical_40,data=.),error=function(e){NA}),
#          model_10_50 = tryCatch(lm(vertical_10 ~ vertical_50,data=.),error=function(e){NA}),
#          model_10_60 = tryCatch(lm(vertical_10 ~ vertical_60,data=.),error=function(e){NA}),
#          model_20_30 = tryCatch(lm(vertical_20 ~ vertical_30,data=.),error=function(e){NA}),
#          model_20_40 = tryCatch(lm(vertical_20 ~ vertical_40,data=.),error=function(e){NA}),
#          model_20_50 = tryCatch(lm(vertical_20 ~ vertical_50,data=.),error=function(e){NA}),
#          model_20_60 = tryCatch(lm(vertical_20 ~ vertical_60,data=.),error=function(e){NA}),
#          model_30_40 = tryCatch(lm(vertical_30 ~ vertical_40,data=.),error=function(e){NA}),
#          model_30_50 = tryCatch(lm(vertical_30 ~ vertical_50,data=.),error=function(e){NA}),
#          model_30_60 = tryCatch(lm(vertical_30 ~ vertical_60,data=.),error=function(e){NA}),
#          model_40_50 = tryCatch(lm(vertical_40 ~ vertical_50,data=.),error=function(e){NA}),
#          model_40_60 = tryCatch(lm(vertical_40 ~ vertical_60,data=.),error=function(e){NA}),
#          model_50_60 = tryCatch(lm(vertical_50 ~ vertical_60,data=.),error=function(e){NA}))
#   }else if(siteHeights[length(siteHeights)]=="vertical_70"){
#     siteDatModels <- siteDat %>% group_by(siteID,week,hour) %>% 
#       do(model_10_20 = tryCatch(lm(vertical_10 ~ vertical_20,data=.),error=function(e){NA}),
#          model_10_30 = tryCatch(lm(vertical_10 ~ vertical_30,data=.),error=function(e){NA}),
#          model_10_40 = tryCatch(lm(vertical_10 ~ vertical_40,data=.),error=function(e){NA}),
#          model_10_50 = tryCatch(lm(vertical_10 ~ vertical_50,data=.),error=function(e){NA}),
#          model_10_60 = tryCatch(lm(vertical_10 ~ vertical_60,data=.),error=function(e){NA}),
#          model_10_70 = tryCatch(lm(vertical_10 ~ vertical_70,data=.),error=function(e){NA}),
#          model_20_30 = tryCatch(lm(vertical_20 ~ vertical_30,data=.),error=function(e){NA}),
#          model_20_40 = tryCatch(lm(vertical_20 ~ vertical_40,data=.),error=function(e){NA}),
#          model_20_50 = tryCatch(lm(vertical_20 ~ vertical_50,data=.),error=function(e){NA}),
#          model_20_60 = tryCatch(lm(vertical_20 ~ vertical_60,data=.),error=function(e){NA}),
#          model_20_70 = tryCatch(lm(vertical_20 ~ vertical_70,data=.),error=function(e){NA}),
#          model_30_40 = tryCatch(lm(vertical_30 ~ vertical_40,data=.),error=function(e){NA}),
#          model_30_50 = tryCatch(lm(vertical_30 ~ vertical_50,data=.),error=function(e){NA}),
#          model_30_60 = tryCatch(lm(vertical_30 ~ vertical_60,data=.),error=function(e){NA}),
#          model_30_70 = tryCatch(lm(vertical_30 ~ vertical_70,data=.),error=function(e){NA}),
#          model_40_50 = tryCatch(lm(vertical_40 ~ vertical_50,data=.),error=function(e){NA}),
#          model_40_60 = tryCatch(lm(vertical_40 ~ vertical_60,data=.),error=function(e){NA}),
#          model_40_70 = tryCatch(lm(vertical_40 ~ vertical_70,data=.),error=function(e){NA}),
#          model_50_60 = tryCatch(lm(vertical_50 ~ vertical_60,data=.),error=function(e){NA}),
#          model_50_70 = tryCatch(lm(vertical_50 ~ vertical_70,data=.),error=function(e){NA}),
#          model_60_70 = tryCatch(lm(vertical_60 ~ vertical_70,data=.),error=function(e){NA}))
#   }
#   
#   fittedModels <- pivot_longer(siteDatModels,cols=4:ncol(siteDatModels),names_to="heights",
#                                values_to="model")
#   fittedModels <- subset(fittedModels,!is.na(model))
#   
#   fittedModelsTidied <- rowwise(fittedModels) %>% summarise(broom::tidy(model))
#   fittedModelsFit <- rowwise(fittedModels) %>% summarise(glance(model))
#   fittedModels <- cbind(fittedModels,fittedModelsFit$r.squared)
#   
#   newDat <- matrix(nrow=nrow(fittedModelsTidied),ncol=5)
#   
#   ct <- 1
#   for(i in 1:nrow(fittedModels)){
#     if(i%%100==0){
#       print(i)
#     }
#     newDat[ct,1] <- as.character(fittedModels[i,1])
#     newDat[ct,2] <- as.character(fittedModels[i,2])
#     newDat[ct,3] <- as.character(fittedModels[i,3])
#     newDat[ct,4] <- as.character(fittedModels[i,4])
#     newDat[ct,5] <- as.character(fittedModels[i,6])
#     ct <- ct + 1
#     newDat[ct,1] <- as.character(fittedModels[i,1])
#     newDat[ct,2] <- as.character(fittedModels[i,2])
#     newDat[ct,3] <- as.character(fittedModels[i,3])
#     newDat[ct,4] <- as.character(fittedModels[i,4])
#     newDat[ct,5] <- as.character(fittedModels[i,6])
#     ct <- ct + 1
#   }
#   
#   colnames(newDat) <- c('siteID','week','hour','heights','r.squared')
#   fittedModelSummary <- cbind(newDat,fittedModelsTidied)
#   allFittedModelSummaries <- rbind(allFittedModelSummaries,fittedModelSummary)
# }
# write.csv(allFittedModelSummaries,file=paste0(dataName,"towerHeight_LRfits.csv"),quote=FALSE,row.names = FALSE)
