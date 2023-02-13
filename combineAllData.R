#Load all NEON data 
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

#NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----
p=2 #For only breaking leaf buds 
phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p]) %>%
  dplyr::select(siteID,year,dayOfYear,individualID,canopyPosition,plantStatus,stemDiameter,
                maxCanopyDiameter,percentCover,height,scientificName,growthForm,Mycorrhizal.type)
#write.csv(phenoDat,file="phenoDat.csv",row.names=FALSE,quote=FALSE)

print("loaded PhenoDat")
#NEON Soil Physical and Chemical Properties ----
dataName <- "NEON_soilProperties"
outFileName <- paste0(dataName,"ALLdata.csv")
soilPropDat <- read.csv(file=paste0(dataPath,outFileName))
soilPropDat <- soilPropDat %>% dplyr::select(-c(decimalLatitude,decimalLongitude,elevation))

allComDat <- left_join(phenoDat,soilPropDat,by=c('siteID','year'))
print("loaded soilProp")
# #NEON Root Characteristics ----
# dataName="NEON_Roots"
# outFileName <- paste0(dataName,"ALLdata.csv")
# rootDat <- read.csv(file=paste0(dataPath,outFileName))
# colnames(rootDat)[3:12] <- paste0('root_',colnames(rootDat[3:12]))
# allComDat <- left_join(allComDat,rootDat,by=c('siteID','year'))
# print("loaded rootDat")
# #NEON Litterfall ----
# dataName="NEON_litterfall"
# outFileName <- paste0(dataName,"ALLdata.csv")
# litterDat <- read.csv(file=paste0(dataPath,outFileName)) %>%
#   subset(functionalGroup%in%c("Leaves","Needles")) %>%
#   dplyr::select(-c(plotType,setDate.x,setDate.y,setDate,functionalGroup))
# colnames(litterDat)[3:10] <- paste0('litter_',colnames(litterDat)[3:10])
# allComDat <- left_join(allComDat,litterDat,by=c('siteID','year'))
# print("loaded litterDat")
# #NEON Foliar Traits ----
# dataName="NEON_plantFoliarTraits"
# outFileName <- paste0(dataName,"ALLdata.csv")
# foliarTraitDat <- read.csv(file=paste0(dataPath,outFileName)) %>% dplyr::select(-plotType)
# colnames(foliarTraitDat)[3:24] <- paste0('foliarTrait_',colnames(foliarTraitDat)[3:24])
# allComDat <- left_join(allComDat,foliarTraitDat,by=c('siteID','year'))
# print("loaded foliarDat")

#NEON Single Air Temp at Various Heights ----
dataName="NEON_SingleAirTemperature"
funName="mean"
tempDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
tempDat <- tempDat %>% mutate(year=lubridate::year(tempDat$date)) %>% dplyr::select(-date,X)

#rm(phenoDat,foliarTraitDat,litterDat,rootDat,soilPropDat)
allComDat <- left_join(allComDat,tempDat,by=c('siteID','year'))

#NEON Precipitation ----
dataName="NEON_PrecipitationData"
funName="sum"
precipDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
precipDat <- precipDat %>% mutate(year=lubridate::year(date)) %>% dplyr::select(-date,X)

rm(phenoDat,tempDat,soilPropDat)
allComDat <- left_join(allComDat,precipDat,by=c('siteID','year'))

write.csv(allComDat,file="allComDat.csv",row.names=FALSE,quote=FALSE)
print("Done")

# tempMaxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('max'),".csv"))
# tempMinDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('min'),".csv"))
# 
# #NEON Precipitation ----
# dataName="NEON_PrecipitationData"
# precipDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('sum'),".csv"))
# 
# #NEON PAR ----
# dataName="NEON_PAR"
# PARMeanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('mean'),".csv"))
# PARMaxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('max'),".csv"))
# PARMinDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('min'),".csv"))
# 
# #NEON RH ----
# dataName="NEON_relativeHumidity"
# RHMeanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('mean'),".csv"))
# RHMaxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('max'),".csv"))
# RHMinDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('min'),".csv"))
# 
# #NEON Windspeed ----
# dataName="NEON_Windspeed"
# wsMeanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('mean'),".csv"))
# wsMaxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('max'),".csv"))
# wsMinDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('min'),".csv"))
# 
# #NEON Soil Temp ----
# dataName="NEON_SoilTemp"
# soilTempMeanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('mean'),".csv"))
# soilTempMaxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('max'),".csv"))
# soilTempMinDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('min'),".csv"))
# 
# #Shade Tolerance ----
# 
# 
# 
