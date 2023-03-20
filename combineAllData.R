#Load all NEON data 
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')
#library(data.table)
source('computeWeeklyMetData.R')
library('purrr')

#NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----
p=2 #For only breaking leaf buds 
p=3 #For colored leaves
#phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllStatus_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
phenoDat$phenoStatus <- "no"
if(p==2){
  phenoDat$phenoStatus[phenoDat$phenophaseIntensity=="11 to 100"] <- "yes"
}else if(p==3){
  phenoDat$phenoStatus[phenoDat$phenophaseIntensity=="5-24%"] <- "yes"
}


subPhenoDat <- phenoDat %>% filter(phenoStatus=="yes") %>%
  group_by(year,individualID) %>% slice(1:1) %>% 
  dplyr::select(year,date,individualID) 
phenoDat <- left_join(phenoDat,subPhenoDat,by=c('year','individualID'))
phenoDat <- phenoDat %>% filter(date.x <= date.y,!is.na(height))
names(phenoDat)[names(phenoDat)=="date.x"] <- "date"

phenoDat <- phenoDat %>%
  dplyr::select(siteID,year,date,dayOfYear,individualID,phenoStatus,canopyPosition,plantStatus,stemDiameter,
                maxCanopyDiameter,percentCover,height,scientificName,growthForm,Mycorrhizal.type)
phenoDat$year <- as.numeric(phenoDat$year)
rm(subPhenoDat)
# phenoDat <- phenoDat %>% filter(phenophaseIntensity == mediumIntensity_phenophases[p]) %>%
#   dplyr::select(siteID,year,date,dayOfYear,individualID,canopyPosition,plantStatus,stemDiameter,
#                 maxCanopyDiameter,percentCover,height,scientificName,growthForm,Mycorrhizal.type)
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
rm(phenoDat,soilPropDat,rootDat,litterDat,foliarTraitDat)
allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                         dataName=dataName,dataPath=dataPath,funName=funName)
allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
rm(allWeekDatList)

#tempDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
#tempDat <- allWeeks %>% mutate(year=lubridate::year(date)) %>% dplyr::select(-date)

tempDat <- pivot_wider(allWeeks,names_from=verticalPosition,values_from=c(GDD,CDD))
rm(allWeeks)
#rm(phenoDat,foliarTraitDat,litterDat,rootDat,soilPropDat)

#Edit NEON Temperature ----
metList=apply(X=tempDat,MARGIN=1,FUN=determineVerticalValue_top)#Calculates for each row 

tempDat$CDD_top <- metList[1,]
tempDat$GDD_top <- metList[2,]

allComDat <- left_join(allComDat,tempDat,by=c('siteID','date'))

metList=apply(X=allComDat,MARGIN=1,FUN=determineVerticalValue_closest) #Calculates for each row 

if(is.null(nrow(metList))){
  metList_unlisted <- data.frame(t(sapply(metList, function(x) x[1:2])))
  metList_unlisted <- t(metList_unlisted)
  metList <- metList_unlisted
  rm(metList_unlisted)
}
allComDat$CDD_closest <- metList[1,]
allComDat$GDD_closest <- metList[2,]

print("Finished joining temp")
rm(tempDat)

#NEON Precipitation ----
dataName="NEON_PrecipitationData"
funName="sum"
# precipDat <- read.csv(paste0(dataPath,dataName,"_computedWeeklyData_",funName,"_",'HARV',".csv"))
allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                         dataName=dataName,dataPath=dataPath,funName=funName)
allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
rm(allWeekDatList)

precipDat <- allWeeks
rm(allWeeks)

allComDat <- left_join(allComDat,precipDat,by=c('siteID','date'))
rm(precipDat)

#NEON Soil Temp ----
dataName="NEON_SoilTemp"
funName="mean"
allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                         dataName=dataName,dataPath=dataPath,funName=funName)
allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
rm(allWeekDatList)

soilTempDat <- pivot_wider(allWeeks,names_from=verticalPosition,values_from=c(soil_GDD,soil_CDD))
rm(allWeeks)

allComDat <- left_join(allComDat,soilTempDat,by=c('siteID','date'))
rm(soilTempDat)

#Photoperiod ----
photoperiodDat <- read.csv(file=paste0(dataPath,'NEON_sunlightTimes.csv'),header=TRUE)
photoperiodDat <- photoperiodDat %>% dplyr::select(siteID,date,daylength)
allComDat <- left_join(allComDat,photoperiodDat,by=c('siteID','date'))

#Shade Tolerance ----
shadeTol <- read.csv(file=paste0(dataPath,'shadeToleranceSpecies.csv'),header=TRUE)
names(shadeTol)[names(shadeTol)=="X...Species"] <- "newScientificName"
shadeTol <- shadeTol %>% dplyr::select(newScientificName,Shade.tolerance,
                                       Drought.tolerance,Waterlogging.tolerance) 
shadeTol$Shade.tolerance <- sapply(strsplit(as.character(shadeTol$Shade.tolerance), "±"), `[`, 1)
shadeTol$Drought.tolerance <- sapply(strsplit(as.character(shadeTol$Drought.tolerance), "±"), `[`, 1)
shadeTol$Waterlogging.tolerance <- sapply(strsplit(as.character(shadeTol$Waterlogging.tolerance), "±"), `[`, 1)
shadeTol <- shadeTol[2:nrow(shadeTol),]
genus <- sapply(strsplit(as.character(allComDat$scientificName), " "), `[`, 1)
species <- sapply(strsplit(as.character(allComDat$scientificName), " "), `[`, 2)
allComDat$newScientificName <- paste(genus,species)

allComDat <- left_join(allComDat,shadeTol,by=c('newScientificName'))

#Site climate characteristics
siteData_sub <- siteData %>% dplyr::select(field_site_id,field_latitude,field_longitude,field_mean_elevation_m,
                                           field_mean_annual_temperature_C,field_mean_annual_precipitation_mm,
                                           field_mean_canopy_height_m)
names(siteData_sub)[names(siteData_sub)=="field_site_id"] <- "siteID"
allComDat <- left_join(allComDat,siteData_sub,by='siteID')

#Editing of Data Object ----
allComDat <- allComDat %>% dplyr::select(-c(percentCover,soilTemp_,soilTemp_O,
                                            soilTemp_M,litterDepth_,soilMoisture_,
                                            newScientificName,date))
allComDat <- allComDat %>% filter(year<2022)

save(allComDat,file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData"))
#Machine Learning Modeling ----

#readr::write_csv(allComDat,file="allComDat.csv")
print("Done Combining Data")
# library('h2o')
# h2o.init()
allComDat_sub <- allComDat %>% filter(growthForm=="Deciduous broadleaf") %>% 
  dplyr::select(-individualID,-scientificName,-growthForm)
library(randomForest)
library(caTools)
library('xgboost')

set.seed(0)

sample = sample.split(allComDat_sub$dayOfYear, SplitRatio = .75)
train = subset(allComDat_sub, sample == TRUE)
test  = subset(allComDat_sub, sample == FALSE)
train_x <- data.matrix(train[,-3])
train_y <- data.matrix(train[,3])

test_x <- data.matrix(test[,-3])
test_y <- data.matrix(test[,3])

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)
pred_y <- predict(model,test_x)
mdl <- lm(pred_y~test_y)

importance_matrix <- xgb.importance(
  feature_names = colnames(train_x), 
  model = model
)

pdf(file="importancePlot.pdf",
     width=6,height=20)
xgb.plot.importance(importance_matrix)
dev.off()

# rf <- randomForest(
#   dayOfYear ~ .,
#   data=train,
#   na.action = na.roughfix
# )

test <- as.matrix(cbind(c(1,2,3,4,5),c(1,2,3,4,5)))
test2 <- list(data=test[,1],label=test[,2])




bstSparse <- xgboost(data = test2$data, label = test2$label, 
                     max.depth = 2, eta = 1, nthread = 2, nrounds = 2, 
                     objective = "binary:logistic")


# allComDat_sub <- as.h2o(allComDat_sub)
# 
# #Create Random Forest Model ----
# splits <- h2o.splitFrame(data = allComDat_sub,
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

