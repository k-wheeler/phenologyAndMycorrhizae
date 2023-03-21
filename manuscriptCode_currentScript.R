args = commandArgs(trailingOnly=TRUE)
print(args)
my_task_id=as.numeric(args[1])
num_tasks=as.numeric(args[2])
fnames <- 47:1
my_fnames=fnames[seq((my_task_id+1),47,num_tasks)]

for(s in my_fnames){
  #Load all NEON data 
  source('sharedVariables.R')
  source('NEON_Data_DownloadAndProcess.R')
  #library(data.table)
  dataName="NEON_SoilMoisture"
  NEON_ID='DP1.00094.001'
  
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  print(s)
  savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  stackByTable(paste0(savePath,'/filesToStack',IDnum))
}


# #NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----
# p=2 #For only breaking leaf buds 
# phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))   
# phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p]) %>%
#   dplyr::select(siteID,year,dayOfYear,individualID,canopyPosition,plantStatus,stemDiameter,
#                 maxCanopyDiameter,percentCover,height,scientificName,growthForm,Mycorrhizal.type)
# #write.csv(phenoDat,file="phenoDat.csv",row.names=FALSE,quote=FALSE)
# phenoDat <- phenoDat %>% filter(growthForm=="Deciduous broadleaf")
# 
# print("loaded PhenoDat")
# #NEON Soil Physical and Chemical Properties ----
# dataName <- "NEON_soilProperties"
# outFileName <- paste0(dataName,"ALLdata.csv")
# soilPropDat <- read.csv(file=paste0(dataPath,outFileName))
# soilPropDat <- soilPropDat %>% dplyr::select(-c(decimalLatitude,decimalLongitude,elevation))
# 
# allComDat <- left_join(phenoDat,soilPropDat,by=c('siteID','year'))
# print("loaded soilProp")
# 
# #NEON Single Air Temp at Various Heights ----
# dataName="NEON_SingleAirTemperature"
# funName="mean"
# tempDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
# tempDat <- tempDat %>% mutate(year=lubridate::year(tempDat$date)) %>% dplyr::select(-date,X)
# 
# #rm(phenoDat,foliarTraitDat,litterDat,rootDat,soilPropDat)
# allComDat <- left_join(allComDat,tempDat,by=c('siteID','year'))
# print("Finished joining temp")
# 
# #NEON Precipitation ----
# dataName="NEON_PrecipitationData"
# funName="sum"
# precipDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
# precipDat <- precipDat %>% mutate(year=lubridate::year(date)) %>% dplyr::select(-c(date,X))
# precipDat <- pivot_wider(precipDat,names_from=week,values_from=sumPrecip)
# 
# rm(phenoDat,tempDat,soilPropDat)
# 
# allComDat <- left_join(allComDat,precipDat,by=c('siteID','year'))
# 
# allComDat$X.y <- NULL
# allComDat$X.x <- NULL
# 
# splits <- h2o.splitFrame(data = allComDat,
#                          ratios = c(0.8),  #partition data into 80% and 20% chunks
#                          seed = 198)
# train <- splits[[1]]
# test <- splits[[2]]
# 
# rf <- h2o.randomForest(x = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#                        y = c("Species"),
#                        training_frame = train,
#                        model_id = "our.rf",
#                        seed = 1234)

