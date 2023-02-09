calculateWeeklyWeather <- function(X,phenoRow,dataName,dat){
  weekDat <- dat %>% filter(siteID==phenoRow[1],as.Date(date)%in%seq((as.Date(as.character(phenoRow[2]))-X*7-6),
                                                                     (as.Date(as.character(phenoRow[2]))-X*7),by="day"))
  if(dataName%in%c('NEON_SingleAirTemperature')){
    weekDat <- weekDat %>% group_by(verticalPosition) %>% summarise(GDD=calculateGDD(tempSingleMean),
                                                                    CDD=calculateCDD(tempSingleMean)) %>%
      mutate(week=X,siteID=phenoRow[1],date=phenoRow[2])
  }
  return(weekDat)
}
calculateAllWeeklyWeather <- function(X,dataName,dat,nWeeks){
  allWeekDatList <- lapply(X=(0:(nWeeks-1)),FUN=calculateWeeklyWeather,phenoRow=X,dataName=dataName,dat=dat)
  allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
  if(dataName%in%c('NEON_SingleAirTemperature')){
    allWeeks <- pivot_wider(allWeeks,names_from=c(1,4),values_from=2:3)
  }
  return(allWeeks)
}

computeWeeklyMetDataFiles <- function(p,siteID,dataName,dataPath,funName,nWeeks){
  #p=2 #For only breaking leaf buds 
  phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
  phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p]) %>%
    dplyr::select(siteID,date) %>% unique()
  
  metDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",funName,".csv"))
  
  metList=apply(X=phenoDat,MARGIN=1,FUN=calculateAllWeeklyWeather,
                 dataName=dataName,dat=metDat,nWeeks=nWeeks)
  metList_unlisted <-rbindlist(metList,fill=TRUE)
  write.csv(paste0(dataPath,dataName,"_computedWeeklydata_",funName,"_",siteID,".csv"))
  
}






