args = commandArgs(trailingOnly=TRUE)
print(args)
my_task_id=as.numeric(args[1])
num_tasks=as.numeric(args[2])
fnames <- 1:73
my_fnames=fnames[seq((my_task_id+1),73,num_tasks)]

for(f in my_fnames){
  library('ncdf4')
  library('suncalc')
  library('tidyverse')
  continent <- "USA"
  if(continent=="europe"){
    maxLat <- 70#max(includedStations$latitude) + 0.5
    minLat <- 35#min(includedStations$latitude) - 0.5
    maxLon <- 42#max(includedStations$longitude) + 0.5
    minLon <- -15#min(includedStations$longitude) - 0.5
  }else if(continent=="USA"){
    maxLat <- 51
    minLat <- 25
    maxLon <- (-63)
    minLon <- (-130)
  }
  
  #ncFiles <- paste0("HARV_ERA5/",dir(path="HARV_ERA5",pattern="nc"))
  ncFiles <- paste0('Data/ERA5_phenoObs/',dir(path='Data/ERA5_phenoObs/',pattern=continent))
  phenoSites <- read.csv('allPhenoSites.csv')
  phenoSites$roundedLat <- round(c(phenoSites$latitude) * 4) / 4
  phenoSites$roundedLon <- round(c(phenoSites$longitude) * 4) / 4
  groupedSites <- phenoSites %>% group_by(roundedLat,roundedLon) %>% summarise(n=n())
  groupedSites$n <- NULL
  colnames(groupedSites) <- c('latitude','longitude')
  phenoSites <- groupedSites
  phenoSites <- phenoSites %>% filter(latitude<maxLat & latitude>minLat & longitude < maxLon & longitude > minLon)
  
  unlistAndSave <- function(dat,driverName,latLongYear){
    dat=matrix(unlist(dat), ncol = 365,byrow = TRUE)
    colnames(dat) <- seq(1,365)
    dat <- cbind(latLongYear,dat)
    
    write.csv(dat,file=paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"),quote=FALSE,row.names = FALSE)
  }
  
  print(f)
  nc <- nc_open(ncFiles[f])
  
  time <- as.integer(ncdf4::ncvar_get(nc, "time"))
  time <- as.POSIXct(time*3600, origin = "1900-01-01",tz = "GMT")
  time <- as.Date(time)
  dates <- unique(time)
  yr <- lubridate::year(dates[1])
  latLongYear <- cbind(phenoSites[,c('latitude','longitude')],yr)
  colnames(latLongYear) <- c('lat','lon','year')
  
  latDim <- (ncdf4::ncvar_get(nc, "latitude"))
  lonDim <- (ncdf4::ncvar_get(nc, "longitude"))
  
  #Cummulative Total Precipitation
  driverName <- 'cumP'
  if(!file.exists(paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"))){
    
    v <- "tp"
    print(v)
    allDat <- ncvar_get(nc,varid=v)
    
    allCumP=lapply(1:nrow(phenoSites),function(X){
      lon=phenoSites$longitude[X]
      lat=phenoSites$latitude[X]
      
      dat <- allDat[which(lonDim==lon),which(latDim==lat),,]
      
      dat <- colMeans(dat)
      datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
        summarise(mean=mean(var),min=min(var),max=max(var),sum=sum(var)) %>%
        pivot_longer(cols=2:5,names_to="funName",values_to="value") %>%
        mutate(var=v)
      
      datTime$cumP <- NA
      datTime <- datTime %>% filter(var=="tp", funName=="sum")
      cumP <- rep(NA,nrow(datTime))
      cumP[1] <- datTime$value[1]
      for(t in 2:nrow(datTime)){
        cumP[t] <- cumP[(t-1)]+datTime$value[t]
      }
      return(cumP[1:365])
      
    })
    unlistAndSave(dat=allCumP,driverName="cumP",latLongYear=latLongYear)
  }
  
  #Mean Daily GDD
  v <- 't2m'
  print(v)
  allDat <- ncvar_get(nc,varid=v)
  driverName <- 'GDD'
  if(!file.exists(paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"))){
    allGDD=lapply(1:nrow(phenoSites),function(X){
      lon=phenoSites$longitude[X]
      lat=phenoSites$latitude[X]
      
      dat <- allDat[which(lonDim==lon),which(latDim==lat),,]
      
      dat <- colMeans(dat)
      datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
        summarise(mean=mean(var),min=min(var),max=max(var),sum=sum(var)) %>%
        pivot_longer(cols=2:5,names_to="funName",values_to="value") %>%
        mutate(var=v)
      
      datTime <- datTime %>% filter(funName=="mean")
      datTime$value <- datTime$value - 273 #Convert from K to C
      datTime$GDD_offset <- unlist(lapply(datTime$value,function(X){
        return(max(X-0,0))
      }))
      
      GDD <- rep(NA,nrow(datTime))
      GDD[1:45] <- NA
      
      for(t in 46:nrow(datTime)){
        GDD[t] <- sum(datTime$GDD_offset[(t-45):(t-1)])
      }
      return(GDD[1:365])
    })
    unlistAndSave(dat=allGDD,driverName="GDD",latLongYear=latLongYear)
  }
  
  #Mean Daily CDD
  driverName <- 'CDD'
  if(!file.exists(paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"))){
    
    print('CDD')
    allCDD=lapply(1:nrow(phenoSites),function(X){
      lon=phenoSites$longitude[X]
      lat=phenoSites$latitude[X]
      
      dat <- allDat[which(lonDim==lon),which(latDim==lat),,]
      
      dat <- colMeans(dat)
      datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
        summarise(mean=mean(var),min=min(var),max=max(var),sum=sum(var)) %>%
        pivot_longer(cols=2:5,names_to="funName",values_to="value") %>%
        mutate(var=v)
      
      datTime <- datTime %>% filter(funName=="mean")
      datTime$value <- datTime$value - 273 #Convert from K to C
      datTime$CDD_offset <- unlist(lapply(datTime$value,function(X){
        return(max(20-X,0))
      }))
      
      CDD <- rep(NA,nrow(datTime))
      CDD[1:14] <- NA
      
      for(t in 15:nrow(datTime)){
        CDD[t] <- sum(datTime$CDD_offset[(t-14):(t-1)])
      }
      return(CDD[1:365])
    })
    unlistAndSave(dat=allCDD,driverName="CDD",latLongYear=latLongYear)
  }
  
  #Frost Status
  print('frostStatus')
  driverName <- 'frostStatus'
  if(!file.exists(paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"))){
    
    allFrostStatus=lapply(1:nrow(phenoSites),function(X){
      lon=phenoSites$longitude[X]
      lat=phenoSites$latitude[X]
      
      dat <- allDat[which(lonDim==lon),which(latDim==lat),,]
      
      dat <- colMeans(dat)
      
      datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
        summarise(mean=mean(var),min=min(var),max=max(var),sum=sum(var)) %>%
        pivot_longer(cols=2:5,names_to="funName",values_to="value") %>%
        mutate(var=v)
      
      datTime <- datTime %>% filter(funName=="min")
      datTime$value <- datTime$value - 273
      frostStatus <- rep(NA,length(dates))
      
      lastSpring <- which(datTime$value[1:182]<0)[length(which(datTime$value[1:182]<0))]
      if(length(lastSpring)==0){
        frostStatus[1:181] <- 1
      }else{
        frostStatus[1:(lastSpring)] <- 0
        frostStatus[(lastSpring+1):181] <- 1
      }
      
      firstAutumn <- which(datTime$value[182:length(datTime$value)]<0)[1]+181
      if(is.na(firstAutumn)){
        frostStatus[182:length(frostStatus)] <- 1
      }else{
        frostStatus[182:(firstAutumn-1)] <- 0
        frostStatus[firstAutumn:length(frostStatus)] <- 1
      }
      return(frostStatus[1:365])
    })
    unlistAndSave(dat=allFrostStatus,driverName="frostStatus",latLongYear=latLongYear)
  }
  nc_close(nc)
  
  
  #Day length
  print('daylength')
  driverName <- 'daylength'
  if(!file.exists(paste0("Data/phenoObs_Drivers/",continent,"_",driverName,"_",yr,".csv"))){
    
    allDaylengths=lapply(1:nrow(phenoSites),function(X){
      lon=phenoSites$longitude[X]
      lat=phenoSites$latitude[X]
      
      suntimes <- getSunlightTimes(date=dates,
                                   lat=lat,lon=lon,keep=c("sunrise","sunset"),
                                   tz = "GMT") #GMT because I only care about difference
      daylengths <- as.numeric(difftime(suntimes$sunset,suntimes$sunrise,units="hours"))
      return(daylengths[1:365])
      
    })
    unlistAndSave(dat=allDaylengths,driverName="daylength",latLongYear=latLongYear)
  }
}
