calculateWeeklyWeather <- function(X,phenoRow,dataName,dat){
  weekDat <- dat %>% filter(siteID==as.character(phenoRow[1]),as.Date(date)%in%seq((as.Date(as.character(phenoRow[2]))-X*7-6),
                                                                     (as.Date(as.character(phenoRow[2]))-X*7),by="day"))
  if(dataName%in%c('NEON_SingleAirTemperature')){
    weekDat <- weekDat %>% group_by(verticalPosition) %>% summarise(GDD=calculateGDD(tempSingleMean),
                                                                    CDD=calculateCDD(tempSingleMean))
  }else if(dataName=="NEON_PrecipitationData"){
    weekDat <- weekDat %>% summarise(sumPrecip=sum(precipBulk)) 
  }
  weekDat <- weekDat %>%
    mutate(week=X,siteID=as.character(phenoRow[1]),date=as.character(phenoRow[2]))
  return(weekDat)
}
calculateAllWeeklyWeather <- function(X,dataName,dat,nWeeks){
  allWeekDatList <- lapply(X=(0:(nWeeks-1)),FUN=calculateWeeklyWeather,phenoRow=X,dataName=dataName,dat=dat)
  allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
  if(dataName%in%c('NEON_SingleAirTemperature')){
    allWeeks <- pivot_wider(allWeeks,names_from=c(1,4),values_from=2:3,names_prefix="week")
  }else if(dataName=="NEON_PrecipitationData"){
    allWeeks <- pivot_wider(allWeeks,names_from=week,values_from=sumPrecip,names_prefix="week")
  }
  return(allWeeks)
}

computeWeeklyMetDataFiles <- function(p,siteID,dataName,dataPath,funName,nWeeks){
  #p=2 #For only breaking leaf buds 
  siteName <- siteID
  phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
  phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p],siteID==siteName)
  
  if(nrow(phenoDat)>0){
    phenoDat <- phenoDat %>%
      dplyr::select(siteID,date) %>% unique()
    
    metDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",funName,".csv"))
    
    metList=apply(X=phenoDat,MARGIN=1,FUN=calculateAllWeeklyWeather,
                  dataName=dataName,dat=metDat,nWeeks=nWeeks) #Calculates for each row 
    metList_unlisted <-rbindlist(metList,fill=TRUE)
    write.csv(metList_unlisted,file=paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",siteID,".csv"))
  }
  
}

readWeeklyMetDataFiles <- function(p,X,dataName,dataPath,funName){
  if(file.exists(file=paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",X,".csv"))){
    siteDat <- read.csv(file=paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",X,".csv"),header=TRUE)
    siteDat$X <- NULL
    return(siteDat)
  }
}

calculateTotalWeather <- function(X,dataName,dat){
  phenoRow=X
  weekDat <- dat %>% filter(siteID==as.character(phenoRow[1]),as.Date(date)%in%seq((as.Date(as.character(phenoRow[2]))-120),
                                                                                   (as.Date(as.character(phenoRow[2]))-1),by="day"))
  if(dataName==('NEON_SingleAirTemperature')){
    weekDat <- weekDat %>% group_by(verticalPosition) %>% summarise(GDD=calculateGDD(tempSingleMean),
                                                                    CDD=calculateCDD(tempSingleMean))
  }else if(dataName==('NEON_SoilTemp')){
    weekDat <- weekDat %>% group_by(verticalPosition) %>% summarise(soil_GDD=calculateGDD(soilTempMean_mean),
                                                                    soil_CDD=calculateCDD(soilTempMean_mean))
  }else if(dataName=="NEON_PrecipitationData"){
    weekDat <- weekDat %>% summarise(sumPrecip=sum(precipBulk)) 
  }
  weekDat <- weekDat %>%
    mutate(siteID=as.character(phenoRow[1]),date=as.character(phenoRow[2]))
  return(weekDat)
}

computeTotalMetDataFiles <- function(p,siteID,dataName,dataPath,funName){
  siteName <- siteID
  phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
  phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p],siteID==siteName)
  
  if(nrow(phenoDat)>0){
    phenoDat <- phenoDat %>%
      dplyr::select(siteID,date) %>% unique()
    
    metDat <- read.csv(paste0(dataPath,dataName,'Dailydata_',funName,'_ERAgapFilled.csv'))
    
    metList=apply(X=phenoDat,MARGIN=1,FUN=calculateTotalWeather,
                  dataName=dataName,dat=metDat) #Calculates for each row 
    metList_unlisted <-rbindlist(metList,fill=TRUE)
    write.csv(metList_unlisted,file=paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",siteID,".csv"))
  }
}

readTotalMetDataFiles <- function(p,X,dataName,dataPath,funName){
  if(file.exists(file=paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",X,".csv"))){
    siteDat <- read.csv(file=paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",X,".csv"),header=TRUE)
    siteDat$X <- NULL
    return(siteDat)
  }
}






