#Model to Predict Last Spring and First Autumn Frost
library('ncdf4')
library('suncalc')
library('rjags')
library('runjags')
library('tidyverse')
#ncFiles <- paste0("HARV_ERA5/",dir(path="HARV_ERA5",pattern="nc"))
ncFiles <- paste0('Data/ERA5_phenoObs/',dir(path='Data/ERA5_phenoObs/',pattern="nc"))
phenoSites <- read.csv('allPhenoSites.csv')
phenoSites$roundedLat <- round(c(phenoSites$latitude) * 4) / 4
phenoSites$roundedLon <- round(c(phenoSites$longitude) * 4) / 4
groupedSites <- phenoSites %>% group_by(roundedLat,roundedLon) %>% summarise(n=n())
groupedSites$n <- NULL
colnames(groupedSites) <- c('latitude','longitude')
phenoSites <- groupedSites
phenoSites <- phenoSites %>% filter(latitude<70 & latitude>35 & longitude < 42 & longitude > (-15))

allCDD_spring <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=185)
allGDD_spring <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=185)
allDayLengths_spring <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=185)
allCumP_spring <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=185)
allFrostStatus_spring <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=185)

allCDD_fall <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=186)
allGDD_fall <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=186)
allDayLengths_fall <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=186)
allCumP_fall <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=186)
allFrostStatus_fall <- matrix(nrow=length(phenoSites)*length(ncFiles),ncol=186)

f=1
ct <- 1
#for(f in seq_along(ncFiles)){
for(f in 1:50){
  print(f)
  nc <- nc_open(ncFiles[f])
  
  time <- as.integer(ncdf4::ncvar_get(nc, "time"))
  time <- as.POSIXct(time*3600, origin = "1900-01-01",tz = "GMT")
  time <- as.Date(time)
  
  latDim <- (ncdf4::ncvar_get(nc, "latitude"))
  lonDim <- (ncdf4::ncvar_get(nc, "longitude"))
  s=1
  #for(s in 1:3){
  for(s in 1:nrow(phenoSites)){
    lon=phenoSites$longitude[s]
    lat=phenoSites$latitude[s]
    print(c(lon,lat))
    v <- "tp"
    dat <- ncvar_get(nc,varid=v)
    
    dat <- dat[which(lonDim==lon),which(latDim==lat),,]
    
    dat <- colMeans(dat)
    datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
      summarise(mean=mean(var),min=min(var),max=max(var),sum=sum(var)) %>%
      pivot_longer(cols=2:5,names_to="funName",values_to="value") %>%
      mutate(var=v)
    
    dates <- unique(datTime$date)
    datTime$cumP <- NA
    datTime <- datTime %>% filter(var=="tp", funName=="sum")
    cumP <- rep(NA,nrow(datTime))
    cumP[1] <- datTime$value[1]
    for(t in 2:nrow(datTime)){
      cumP[t] <- cumP[(t-1)]+datTime$value[t]
    }
    
    v <- 't2m'
    dat <- ncvar_get(nc,varid=v)
    dat <- dat[which(lonDim==lon),which(latDim==lat),,]
    
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
    
    datTime$CDD_offset <- unlist(lapply(datTime$value,function(X){
      return(max(20-X,0))
    }))
    
    CDD <- rep(NA,nrow(datTime))
    CDD[1:14] <- NA
    
    for(t in 15:nrow(datTime)){
      CDD[t] <- sum(datTime$CDD_offset[(t-14):(t-1)])
    }
    
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


    
    dayLengths <- numeric()
    #lat <- 42.5378 
    #lon <-  -72.1715
    for(d in 1:length(dates)){
      suntimes <- getSunlightTimes(date=dates[d],
                                   lat=lat,lon=lon,keep=c("sunrise","sunset"),
                                   tz = "GMT") #GMT because I only care about difference
      dayLengths <- c(dayLengths,as.numeric(suntimes$sunset-suntimes$sunrise))
    }
    yr <- lubridate::year(dates[1])
    allCDD_spring[ct,1:185] <- c(lat,lon,yr,CDD[1:182])
    allGDD_spring[ct,1:185] <- c(lat,lon,yr,GDD[1:182])
    allDayLengths_spring[ct,1:185] <- c(lat,lon,yr,dayLengths[1:182])
    allCumP_spring[ct,1:185] <- c(lat,lon,yr,cumP[1:182])
    allFrostStatus_spring[ct,1:185] <- c(lat,lon,yr,frostStatus[1:182])
    
    allCDD_fall[ct,1:186] <- c(lat,lon,yr,CDD[183:365])
    allGDD_fall[ct,1:186] <- c(lat,lon,yr,GDD[183:365])
    allDayLengths_fall[ct,1:186] <- c(lat,lon,yr,dayLengths[183:365])
    allCumP_fall[ct,1:186] <- c(lat,lon,yr,cumP[183:365])
    allFrostStatus_fall[ct,1:186] <- c(lat,lon,yr,frostStatus[183:365])
    ct <- ct + 1
  }
  nc_close(nc)
  
}


write.csv(allCDD_spring,file="allCDD_spring_europe.csv")
write.csv(allGDD_spring,file="allGDD_spring_europe.csv")

write.csv(allCDD_fall,file="allCDD_fall_europe.csv")
write.csv(allGDD_fall,file="allGDD_fall_europe.csv")

write.csv(allDayLengths_spring,file="allDayLengths_spring_europe.csv")
write.csv(allDayLengths_fall,file="allDayLengths_fall_europe.csv")

write.csv(allCumP_spring,file="allCumP_spring_europe.csv")
write.csv(allCumP_fall,file="allCumP_fall_europe.csv")

write.csv(allFrostStatus_spring,file="allFrostStatus_spring_europe.csv")
write.csv(allFrostStatus_fall,file="allFrostStatus_fall_europe.csv")

# 
# 
# calZscore <- function(dat){
#   return((dat-mean(dat,na.rm=TRUE))/sd(dat,na.rm=TRUE))
# }
# dfData <- data.frame(CDD=al)
# 
# allData <- list(CDD=calZscore(allCDD[!is.na(allGDD) & !is.na(allCDD)]),
#                       GDD=calZscore(allGDD[!is.na(allGDD) & !is.na(allCDD)]),
#                       D=calZscore(allDayLengths[!is.na(allGDD) & !is.na(allCDD)]),
#                       P=calZscore(allCumP[!is.na(allGDD) & !is.na(allCDD)]),
#                       y=allFrostStatus[!is.na(allGDD) & !is.na(allCDD)],N=length(allFrostStatus[!is.na(allGDD) & !is.na(allCDD)]))
# 
# # Cumulative precipitation
# #CDD base 20 over a moving window 14 days prior
# #Sum of chilling hours over a moving window 135 to 45 days prior (exclude for now)
# #GDD base 0 over a moving window 45 days to one day prior
# #Day length of prior day
# 
# #Each site has a probability of being post last spring frost or before first autumn frost
# 
# #logit(p) <- X * b
# 
# #Based off of Elmendorf et al. (2019)
# 
# 
# frostModel <- "
# model{
#   # Likelihood
#   for(i in 1:N) {
#     logit(p[i]) <- b0 + b_GDD * GDD[i] + b_D * D[i] + b_P * P[i]
#     y[i] ~ dbern(p[i])
#   }
#   # Priors
#   p0 ~ dbeta(1, 1)
#   b0 <- logit(p0)
#   b_GDD ~ dunif(-5, 5)
#   b_D ~ dunif(-5, 5)
#   b_P ~ dunif(-5, 5)
# }
# "
# 
# j.model <- jags.model(file=textConnection(frostModel),
#                       data=allData,
#                       n.chains=3)
# variableNames <- c('b0','b_GDD','b_D','b_P')
# source('runMCMC_Model.R')
# library('rjags')
# library('runjags')
# jags.out <- runMCMC_Model(j.model,variableNames = variableNames,baseNum = 10000,iterSize = 5000)
# 
# jags.mat <- data.frame(as.matrix(jags.out))
