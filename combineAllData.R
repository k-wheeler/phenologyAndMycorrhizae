#Load all NEON data 
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

#NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----
p=2 #For only breaking leaf buds 
phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p])

#NEON Soil Physical and Chemical Properties ----
dataName <- "NEON_soilProperties"
outFileName <- paste0(dataName,"ALLdata.csv")
soilPropDat <- read.csv(file=paste0(dataPath,outFileName)) %>% filter(horizon!="")
soilPropDat <- pivot_wider(soilPropDat,names_from=6,values_from=7:11) %>%
  dplyr::select(-decimalLatitude,decimalLongitude,elevation)

allComDat <- left_join(phenoDat,soilPropDat,by=c('siteID','year'))

#NEON Root Characteristics ----
dataName="NEON_Roots"
outFileName <- paste0(dataName,"ALLdata.csv")
rootDat <- read.csv(file=paste0(dataPath,outFileName))
colnames(rootDat)[3:12] <- paste0('root_',colnames(rootDat[3:12]))
allComDat <- left_join(allComDat,rootDat,by=c('siteID','year'))

#NEON Litterfall ----
dataName="NEON_litterfall"
outFileName <- paste0(dataName,"ALLdata.csv")
litterDat <- read.csv(file=paste0(dataPath,outFileName)) %>% dplyr::select(-c(plotType,setDate.x,setDate.y,setDate))
colnames(litterDat)[3:11] <- paste0('litter_',colnames(litterDat)[3:11])
allComDat <- left_join(allComDat,litterDat,by=c('siteID','year'))

#NEON Foliar Traits ----
dataName="NEON_plantFoliarTraits"
outFileName <- paste0(dataName,"ALLdata.csv")
foliarTraitDat <- read.csv(file=paste0(dataPath,outFileName)) %>% dplyr::select(-plotType)
colnames(foliarTraitDat)[3:24] <- paste0('foliarTrait_',colnames(foliarTraitDat)[3:24])
allComDat <- left_join(allComDat,foliarTraitDat,by=c('siteID','year'))


#NEON Single Air Temp at Various Heights ----
dataName="NEON_SingleAirTemperature"
tempMeanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_",as.character('mean'),".csv"))
allComDat <- allComDat %>% mutate(rowID=row_number())

tempList=apply(X=allComDat,MARGIN=1,FUN=calculateAllWeeklyWeather,
      dataName=dataName,dat=tempMeanDat,nWeeks=5)
tempList_unlisted <-rbindlist(tempList,idcol='rowID',fill=TRUE)
allComDat <- left_join(allComDat,tempList_unlisted,by='rowID')

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
# #ERA5 Data ----
# ERA5Dat <- read.csv(paste0(dataPath,"ERA5_metData.csv"))
# 
# 
# 
