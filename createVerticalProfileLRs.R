source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')
summaryStats <- c('mean','max','min','sum')
#dataName="NEON_SingleAirTemperature"
#varName <- 'tempSingleMean'
#dataName="NEON_PrecipitationData"
#varName="precipBulk"
# dataName="NEON_SoilTemp"
# varName <- 'soilTempMean'
dataName="NEON_SoilMoisture"
varName <- 'soilMoistureMean'

ERAdat <- read.csv(paste0(dataPath,'ERA5_metData.csv'),header=TRUE)
if(dataName=="NEON_SingleAirTemperature"){
  ERAdataName <- 't2m'
  summaryStats <- c("mean","min","max")
}else if(dataName=="NEON_PrecipitationData"){ #Total precipitation
  ERAdataName <- "tp"
  summaryStats <- "sum"
}else if(dataName=="NEON_SoilTemp"){#Soil temperature level 1
  ERAdataName <- "stl1"
}else if(dataName=="NEON_SoilMoisture"){#Volumetric soil water layer 1
  ERAdataName <- "swvl1"
  summaryStats <- c("mean","min","max")
}#else if(dataName==){ #Surface pressure
#   ERAdataName <- "sp"
# }else if(dataName==){ #2m dewpoint temperature
#   ERAdataName <- 'd2m'
# }else if(dataName==){ #10 metre u wind component
#   ERAdataName <- "u10"
# }else if(dataName==){ #10 metre V wind component
#   ERAdataName <- "v10"
#else if(dataName==){#Soil temperature level 2
#   ERAdataName <- "stl2"
# }else if(dataName==){#Soil temperature level 3
#   ERAdataName <- "stl3"
# }else if(dataName==){ #Soil temperature level 4
#   ERAdataName <- "stl4"
# }else if(dataName==){#Volumetric soil water layer 1
#   ERAdataName <- "swvl1"
# }else if(dataName==){#Volumetric soil water layer 2
#   ERAdataName <- "swvl2"
# }else if(dataName==){#Volumetric soil water layer 3
#   ERAdataName <- "swvl3"
# }else if(dataName==){ #Volumetric soil water layer 4
#   ERAdataName <- "swvl4"
# }



ERAdat <- ERAdat %>% filter(var==ERAdataName)
if(ERAdataName%in%c("t2m","stl1")){
  ERAdat$value <- as.numeric(ERAdat$value)-273 #Convert K to C
}

ERAdat <- pivot_wider(ERAdat,names_from=2,values_from=3)


for(summaryStat in summaryStats){
  allData <- read.csv(paste0(dataPath,dataName,'Dailydata_',summaryStat,'.csv'))
  
  allData$week <- floor((lubridate::yday(allData$date)-0.01)/7)+1
  
  allFittedModelSummaries <- matrix(nrow=0,ncol=5)
  
  for(s in (seq_along(NEON_siteNames))){
    siteName <- NEON_siteNames[s]
    print(siteName)
    lat <- siteData$field_latitude[siteData$field_site_id==siteName]
    lon <- siteData$field_longitude[siteData$field_site_id==siteName]
    
    siteDat <- allData %>% filter(siteID==siteName)
    if(nrow(siteDat)>0){
      ERAdatSite <- ERAdat %>% filter(siteID==siteName)
      
      if(summaryStat=="mean"){
        ERAdatSite$ERAValue <- ERAdatSite$mean
      }else if(summaryStat=="min"){
        ERAdatSite$ERAValue <- ERAdatSite$min
      }else if(summaryStat=="max"){
        ERAdatSite$ERAValue <- ERAdatSite$max
      }else if(summaryStat=="sum"){
        ERAdatSite$ERAValue <- ERAdatSite$sum
      }
      ERAdatSite <- ERAdatSite %>% dplyr::select(date,ERAValue)
      
      siteHeights <- unique(siteDat$verticalPosition)
      siteDat$date <- as.Date(siteDat$date)
      ERAdatSite$date <- as.Date(ERAdatSite$date)
      siteDat <- left_join(siteDat,ERAdatSite,by="date")
      if(dataName=="NEON_PrecipitationData"){
        siteDat$verticalPosition <- "top"
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
      }else if(varName=="soilMoisture"){
        siteDat$NEON_value <- siteDat$soilMoistureMean
        siteDat <- siteDat %>% dplyr::select(-soilMoistureMean)
      }
      
      fittedModels <- siteDat %>% group_by(week,verticalPosition) %>% 
        do(model = tryCatch(lm(NEON_value~ERAValue,data=.),error=function(e){NA}))
      
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
      
      fittedModelSummary <- fittedModelSummary %>% dplyr::select(siteID,week,verticalPosition,term,estimate)
      fittedModelSummary <- pivot_wider(fittedModelSummary,names_from = term,values_from = estimate)
      colnames(fittedModelSummary) <- c("siteID","week","verticalPosition","intercept","slope")
      
      #Need to pad with missing week-height combinations
      fullModelSummary <- matrix(nrow=(length(siteHeights)*53),ncol=ncol(fittedModelSummary))
      ct=1
      for(h in seq_along(siteHeights)){
        for(w in 1:53){
          print(ct)
          subDat <- fittedModelSummary %>% filter(week==w,verticalPosition==siteHeights[h])
          if(nrow(subDat)==0){
            newW=w
            while(nrow(subDat)==0){
              #print(newW)
              subDat <- fittedModelSummary %>% filter(week==newW,verticalPosition==siteHeights[h])
              newW <- newW + 1
              if((newW)==54){
                newW=1
              }
            }
            subDat$week <- w
          }
          fullModelSummary[ct,] <- as.character(subDat)
          ct=ct + 1
        }
      }
      
      allFittedModelSummaries <- rbind(allFittedModelSummaries,fittedModelSummary)
    }
  }
  write.csv(allFittedModelSummaries,file=paste0(dataName,"towerHeight_LRfitsWithERA5_",dataName,"_",summaryStat,".csv"),quote=FALSE,row.names = FALSE)
  
  
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

