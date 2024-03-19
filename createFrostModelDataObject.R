#CombineDriver and Sort
library(tidyverse)
library(data.table)
allDrivers <- c('cumP','GDD','CDD','frostStatus','daylength')
ecoRegionDat <- read.csv('allPhenoSites_rounded_withEcoregions.csv')
selectSites <- ecoRegionDat %>% filter(ECO_ID == 647) #Filter to baltic mixed forests

gatherData <- function(driverName){
  driverFiles <- paste0('Data/phenoObs_Drivers/',dir(path="Data/phenoObs_Drivers",pattern=driverName))
  
  allDriverDat=lapply(seq_along(driverFiles),function(X){
    return(read.csv(driverFiles[X]))
  })
  allDriverDat <- rbindlist(allDriverDat)
  
  selectDriverDat <- allDriverDat %>% filter(lat %in% selectSites$lat & lon %in% selectSites$lon)
  print(dim(selectDriverDat))
  selectDriverDat <- selectDriverDat[,-c('lat','lon','year')]
  springDat <- selectDriverDat[,1:182]
  springDat <- calZscore(unlist(c(springDat)))

  fallDat <- selectDriverDat[,183:365]
  fallDat <- calZscore(unlist(c(fallDat)))
  selectDriverDat <- list(fallDat=fallDat,springDat=springDat)
  return(selectDriverDat)
}
calZscore <- function(dat){
  return((dat-mean(dat,na.rm=TRUE))/sd(dat,na.rm=TRUE))
}

allcumP <- gatherData(driverName="cumP")
allGDD <- gatherData(driverName="GDD")
allCDD <- gatherData(driverName="CDD")
allfrostStatus <- gatherData(driverName="frostStatus")
alldaylength <- gatherData(driverName="daylength")

#Combine Spring Data
seasonData <- list()
seasonData$P <- allcumP$springDat
seasonData$CDD <- allCDD$springDat
seasonData$GDD <- allGDD$springDat
seasonData$y <- allfrostStatus$springDat
seasonData$D <- alldaylength$springDat
includeDays <- !is.na(seasonData$P) & !is.na(seasonData$CDD) & !is.na(seasonData$GDD) &
  !is.na(seasonData$y) & !is.na(seasonData$D)

seasonData$P <- seasonData$P[includeDays]
seasonData$CDD <- seasonData$CDD[includeDays]
seasonData$GDD <- seasonData$GDD[includeDays]
seasonData$y <- seasonData$y[includeDays]
seasonData$D <- seasonData$D[includeDays]
springData <- seasonData
save(springData,file='frostModelDataObject_spring.RData')

#Combine Fall Data
seasonData <- list()
seasonData$P <- allcumP$fallDat
seasonData$CDD <- allCDD$fallDat
seasonData$GDD <- allGDD$fallDat
seasonData$y <- allfrostStatus$fallDat
seasonData$D <- alldaylength$fallDat

includeDays <- !is.na(seasonData$P) & !is.na(seasonData$CDD) & !is.na(seasonData$GDD) &
  !is.na(seasonData$y) & !is.na(seasonData$D)

seasonData$P <- seasonData$P[includeDays]
seasonData$CDD <- seasonData$CDD[includeDays]
seasonData$GDD <- seasonData$GDD[includeDays]
seasonData$y <- seasonData$y[includeDays]
seasonData$D <- seasonData$D[includeDays]

fallData <- seasonData
save(fallData,file='frostModelDataObject_fall.RData')










ecoregionsSub <- ecoregions %>% dplyr::select(ECO_NAME,BIOME_NUM,BIOME_NAME,ECO_ID)
ecoRegionDat <- left_join(ecoRegionDat,ecoregionsSub,by="ECO_ID")
ecoregionGroups <- ecoRegionDat %>% group_by(ECO_ID,ECO_NAME,BIOME_NUM,BIOME_NAME) %>% summarize(n=n())
plot(hist(ecoregionGroups$n))
sum(ecoregionGroups$n>25) #17 ecoregions. 
ecoregionGroups <- ecoregionGroups %>% filter(n>24) #16 (12 for one biome)
test <- ecoRegionDat %>% filter(ECO_ID %in% ecoregionGroups$ECO_ID)
#664 European Atlantic mixed forests
#647 Baltic mixed forests


