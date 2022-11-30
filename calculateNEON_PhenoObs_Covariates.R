#Calculate Cummulative Weather to Date from the start of year
library(MuMIn)
library(mgcv)

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
  subDat <- subDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p])
  subDat$year <- as.numeric(subDat$year)
  subDat <- left_join(subDat,allPrecipData[,-3],by=c('siteID','date','year'))
  subDat <- left_join(subDat,allTempData,by=c('siteID','date','year'))
  subDat <- left_join(subDat,allSunlightTimes,by=c('siteID','date'))
  subDat <- subDat %>% filter(year!=2022)
  subDatDB$Mycorrhizal.type <- as.factor(subDatDB$Mycorrhizal.type)
  
  mdl <- lm(dayOfYear ~ cumPrecip + GDD_15 + daylength,data=subDat,na.action = "na.fail")
  
  dredge(mdl)
  
  #subDat <- subDat %>% group_by(individualID) %>% mutate(precip_sd=sd(cumPrecip))
  subDatECM <- subDat %>% filter(Mycorrhizal.type=="EcM",growthForm=="Deciduous broadleaf")
  subDatAM <- subDatDB %>% filter(Mycorrhizal.type=="AM",growthForm=="Deciduous broadleaf")
  subDatDB <- subDatDB %>% filter(growthForm=="Deciduous broadleaf")
  mdl <- lm(dayOfYear ~ cumPrecip*Mycorrhizal.type + GDD_15*Mycorrhizal.type + daylength*Mycorrhizal.type,data=subDat,na.action = "na.fail")

  
  mdl_gam <- mgcv::gam(dayOfYear ~ s(cumPrecip,k=12) + s(GDD_15,k=12) + s(daylength,k=12) + Mycorrhizal.type,
                       data = subDatDB,
                       method="REML",na.action = "na.fail")
  
  mdl_gam <- mgcv::gam(dayOfYear ~ s(cumPrecip,by=Mycorrhizal.type,k=100) + s(GDD_15,by=Mycorrhizal.type,k=100) + s(daylength,by=Mycorrhizal.type,k=100) + siteID,
                       data = subDatDB,
                       method="REML",na.action = "na.fail")
  
  summary(mdl_gam)
  plot.gam(mdl_gam,residuals=TRUE,pch=20,cex=5,shade=TRUE,page=1) #A line that has a steeper slope has a greater effect 
  gam.check(mdl_gam)
  concurvity(mdl_gam,full=TRUE) #If a value is high, set full=FALSE to look at pairwise concurvities 
  
  mdl_gam <- mgcv::gam(ayOfYear ~ siteID  + team + siteID*team + s(horizon)+ s(phenoDate),
                     data = gcc_forecast_subset2,
                     method="REML")
  
  dredge(mdl_gam)
  
  mdl <- lm(dayOfYear ~ cumPrecip + GDD_15 + daylength,data=subDatAM,na.action = "na.fail")
  dredge(mdl)
  
  
  t.test(subDatECM$precip_sd,subDatAM$precip_sd)
  t.test(subDatECM$GDD_10,subDatAM$GDD_10)
  
  plot(density(na.omit(subDatDB$GDD_10)),xlim=c(0,300))
  lines(density(na.omit(subDatAM$GDD_10)),col="red")
  
  lines(density(na.omit(subDat$GDD_10)),col="cyan")
  
  

  
}

pdf("datesOfBudBurst.pdf",height=5,width=5)

for(s in seq_along(NEON_siteNames)[2:47]){
  #subDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
  subSubDat <- subDatDB %>% filter(siteID==NEON_siteNames[s])
  if(nrow(subSubDat)>1){
  plot(density(subSubDat$dayOfYear),main=paste(NEON_siteNames[s],nrow(subSubDat)),xlim=c(1,365))
    
  }
}
dev.off()
