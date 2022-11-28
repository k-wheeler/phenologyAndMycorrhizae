#Calculate Cummulative Weather to Date from the start of year
library(MuMIn)

allPrecipData <- read.csv(paste0(dataPath,"NEON_PrecipitationDataDailydata_gapFilled.csv"))
allPrecipData$year <- lubridate::year(allPrecipData$date)
allPrecipData <- allPrecipData %>% group_by(year,siteID) %>% mutate(cumPrecip=cumsum(precipBulk)) #Calculates the cummulative sum of precip for each site-year for each date

allTempData <- read.csv(paste0(dataPath,"NEON_TemperatureDataDailydata_gapFilled.csv"))
allTempData$year <- lubridate::year(allTempData$date)

allTempData$GDDoffset_0 <- ifelse((allTempData$tempTripleMean-0)<0,0,allTempData$tempTripleMean-0)
allTempData <- allTempData %>% group_by(year,siteID) %>% mutate(GDD_0=cumsum(GDDoffset_0))

allTempData$GDDoffset_5 <- ifelse((allTempData$tempTripleMean-5)<0,0,allTempData$tempTripleMean-5)
allTempData <- allTempData %>% group_by(year,siteID) %>% mutate(GDD_5=cumsum(GDDoffset_5))

allTempData$GDDoffset_10 <- ifelse((allTempData$tempTripleMean-10)<0,0,allTempData$tempTripleMean-10)
allTempData <- allTempData %>% group_by(year,siteID) %>% mutate(GDD_10=cumsum(GDDoffset_10))

allTempData$GDDoffset_15 <- ifelse((allTempData$tempTripleMean-15)<0,0,allTempData$tempTripleMean-15)
allTempData <- allTempData %>% group_by(year,siteID) %>% mutate(GDD_15=cumsum(GDDoffset_15))

allTempData <- allTempData[,-c(5,7,9,11)] #Remove the offset columns to just keep cummulative 

allSunlightTimes <- read.csv(paste0(dataPath,'NEON_sunlightTimes.csv'))[-c(3,4,5,6)]

for(p in seq_along(NEON_phenophase_names)){
  print(NEON_phenophase_names[p])
  subDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
  subDat$year <- as.numeric(subDat$year)
  subDat <- left_join(subDat,allPrecipData[,-3],by=c('siteID','date','year'))
  subDat <- left_join(subDat,allTempData,by=c('siteID','date','year'))
  subDat <- left_join(subDat,allSunlightTimes,by=c('siteID','date'))
  subDat <- subDat %>% filter(year!=2022)
  
  mdl <- lm(dayOfYear ~ cumPrecip + GDD_15 + daylength,data=subDat,na.action = "na.fail")
  
  dredge(mdl)
  
  #subDat <- subDat %>% group_by(individualID) %>% mutate(precip_sd=sd(cumPrecip))
  subDatECM <- subDat %>% filter(Mycorrhizal.type=="EcM",growthForm=="Deciduous broadleaf")
  subDatAM <- subDat %>% filter(Mycorrhizal.type=="AM",growthForm=="Deciduous broadleaf")
  mdl <- lm(dayOfYear ~ cumPrecip + GDD_15 + daylength,data=subDatECM,na.action = "na.fail")
  dredge(mdl)
  
  mdl <- lm(dayOfYear ~ cumPrecip + GDD_15 + daylength,data=subDatAM,na.action = "na.fail")
  dredge(mdl)
  
  
  t.test(subDatECM$precip_sd,subDatAM$precip_sd)
  t.test(subDatECM$GDD_10,subDatAM$GDD_10)
  
  plot(density(na.omit(subDatECM$GDD_10)),xlim=c(0,1000))
  lines(density(na.omit(subDatAM$precip_sd)),col="red")
  
  lines(density(na.omit(subDat$GDD_10)),col="cyan")
  
  

  
}

