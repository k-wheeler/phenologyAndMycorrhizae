#CombineDriver and Sort
library(tidyverse)
library(data.table)
allDrivers <- c('cumP','GDD','CDD','frostStatus','daylength')
ecoRegionDat <- read.csv('allPhenoSites_rounded_withEcoregions.csv')
ecoregionIDs <- read.table('ecoregionIDs.txt',sep=";",header=TRUE)
ecoregionIDs$ECO_ID <- as.numeric(ecoregionIDs$ECO_ID)

ecoRegionDat <- left_join(ecoRegionDat,ecoregionIDs,by="ECO_ID")
ecoregionGroups <- ecoRegionDat %>% group_by(ECO_ID,ECO_NAME,BIOME_NUM,BIOME_NAME) %>% summarize(n=n())
#plot(hist(ecoregionGroups$n))
#sum(ecoregionGroups$n>25) #17 ecoregions. 
ecoregionGroups <- ecoregionGroups %>% filter(n>24) #16 (12 for one biome)
selectEcoIDs <- na.omit(ecoregionGroups$ECO_ID)

ecoRegionDat <- ecoRegionDat %>% mutate(lat_lon=paste(latitude,longitude,sep="_"))

selectSites <- ecoRegionDat %>% filter(ECO_ID %in% selectEcoIDs) %>% dplyr::select(lat_lon, ECO_ID) %>% unique()

calZscore <- function(dat){
  return((dat-mean(dat,na.rm=TRUE))/sd(dat,na.rm=TRUE))
}

gatherData <- function(driverName){
  driverFiles <- paste0('Data/phenoObs_Drivers/',dir(path="Data/phenoObs_Drivers",pattern=driverName))
  
  allDriverDat=lapply(seq_along(driverFiles),function(X){
    return(read.csv(driverFiles[X]))
  })
  allDriverDat <- rbindlist(allDriverDat)
  allDriverDat <- allDriverDat %>% mutate(lat_lon=paste(lat,lon,sep="_"))
  
  selectDriverDat <- allDriverDat %>% filter(lat_lon %in% selectSites$lat_lon)
  selectDriverDat <- left_join(selectDriverDat,selectSites,by="lat_lon")
  print(dim(selectDriverDat))
  names(selectDriverDat)[4:368] <- seq(1,365)
  selectDriverDat=pivot_longer(selectDriverDat,cols = seq(4,368),names_to="DOY",values_to = "driverValue")
  selectDriverDat$DOY <- as.numeric(selectDriverDat$DOY)
  
  if(driverName !="frostStatus"){
    selectDriverDat$driverValue <- calZscore(selectDriverDat$driverValue)
  }
  return(selectDriverDat)
  # selectDriverDat <- list(springDat=processSeasonDat(selectDriverDat = selectDriverDat,season = "spring"),
  #                         fallDat=processSeasonDat(selectDriverDat = selectDriverDat,season = "fall"))
  # return(selectDriverDat)
}

processSeasonDat <- function(selectDriverDat,season){
  if(season=="spring"){
    seasonDat <- selectDriverDat %>% filter(DOY %in% seq(1,182))
    set.seed(1)
  }else if(season=="fall"){
    seasonDat <- selectDriverDat %>% filter(DOY %in% seq(183,365))
    set.seed(2)
  }else{
    print("Need season value of 'spring' or 'fall'")
    return()
  }
  
  seasonDat <- seasonDat %>% group_by(ECO_ID) %>% 
    slice_sample(n=1500) %>% 
    mutate(rowID=row_number()) %>% 
    dplyr::select(rowID,ECO_ID,driverValue) %>%
    pivot_wider(values_from = driverValue,names_from = ECO_ID) %>%
    dplyr::select(-rowID)
  
  return(seasonDat)
}

allcumP <- gatherData(driverName="cumP")
allGDD <- gatherData(driverName="GDD")
allCDD <- gatherData(driverName="CDD")
allfrostStatus <- gatherData(driverName="frostStatus")
alldaylength <- gatherData(driverName="daylength")

#Remove rows with NAs
includeDays <- !is.na(allcumP$driverValue) & !is.na(allGDD$driverValue) & !is.na(allCDD$driverValue) &
  !is.na(allfrostStatus$driverValue) & !is.na(alldaylength$driverValue)

allcumP <- allcumP[includeDays,]
allGDD <- allGDD[includeDays,]
allCDD <- allCDD[includeDays,]
allfrostStatus <- allfrostStatus[includeDays,]
alldaylength <- alldaylength[includeDays,]

#Group By ECO_ID, sample 1500 rows for each ECO_ID, and format so each driver is a matrix with values in rows and each column a different ECO_ID

for(season in c('spring','fall')){
  seasonData <- list()
  seasonData$P <- processSeasonDat(allcumP,season = season)
  seasonData$GDD <- processSeasonDat(allGDD,season = season)
  seasonData$CDD <- processSeasonDat(allCDD,season = season)
  seasonData$D <- processSeasonDat(alldaylength,season = season)
  seasonData$y <- processSeasonDat(allfrostStatus,season = season)
  save(seasonData,file=paste0('frostModelDataObject_',season,'.RData'))
}


#test <- ecoRegionDat %>% filter(ECO_ID %in% ecoregionGroups$ECO_ID)
#664 European Atlantic mixed forests
#647 Baltic mixed forests


