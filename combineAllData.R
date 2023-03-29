#Load all NEON data 
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')
source('computeWeeklyMetData.R')

p=2 #For only breaking leaf buds 
p=3 #For colored leaves
baseTemp=20

#NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----
phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllStatus_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
phenoDat$phenoStatus <- "no"
if(p==2){
  phenoDat$phenoStatus[phenoDat$phenophaseIntensity=="11 to 100"] <- "yes"
}else if(p==3){
  phenoDat$phenoStatus[phenoDat$phenophaseIntensity=="5-24%"] <- "yes"
}

subPhenoDat <- phenoDat %>% filter(phenoStatus=="yes") %>%
  group_by(year,individualID,height) %>% slice(1:1) %>% 
  dplyr::select(year,date,individualID,height) 
phenoDat <- left_join(phenoDat,subPhenoDat,by=c('year','individualID','height'))
phenoDat <- phenoDat %>% filter(date.x <= date.y,!is.na(height))
names(phenoDat)[names(phenoDat)=="date.x"] <- "date"

phenoDat <- phenoDat %>%
  dplyr::select(siteID,year,date,dayOfYear,individualID,phenoStatus,canopyPosition,plantStatus,
                height,scientificName,growthForm,Mycorrhizal.type)
phenoDat$year <- as.numeric(phenoDat$year)
rm(subPhenoDat)
print("loaded PhenoDat")

phenoDat <- phenoDat %>% 
  filter(growthForm=="Deciduous broadleaf") %>% 
  group_by(siteID,year,date,dayOfYear,individualID,phenoStatus,canopyPosition,plantStatus,height,scientificName,
           growthForm, Mycorrhizal.type) %>% slice(1:1)

#Spring Dates to Calculate Leaf Age ----
p=2
springDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllStatus_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
springDat$phenoStatus <- "no"
# springDat$phenoStatus[springDat$phenophaseIntensity %in% 
#                         c("11 to 100")] <- "yes"
springDat$phenoStatus[springDat$phenophaseIntensity %in%
                        c("11 to 100","101 to 1000","1001 to 10000",">10000")] <- "yes"

subPhenoDat <- springDat %>% filter(phenoStatus=="yes") %>%
  group_by(year,individualID,height) %>% dplyr::slice(1:1) %>%
  dplyr::select(individualID,year,height,dayOfYear) %>% mutate(year=as.numeric(year),
                                                               springDayOfYear=dayOfYear,
                                                               dayOfYear=NULL)

allComDat <- left_join(phenoDat,subPhenoDat,by=c('individualID','year','height'))
#allComDat <- left_join(allComDat,subPhenoDat,by=c('individualID','year','height'))
p=3
#NEON Soil Physical and Chemical Properties ----
dataName <- "NEON_soilProperties"
outFileName <- paste0(dataName,"ALLdata.csv")
soilPropDat <- read.csv(file=paste0(dataPath,outFileName))
soilPropDat <- soilPropDat %>% dplyr::select(-c(decimalLatitude,decimalLongitude,elevation))

allComDat <- left_join(allComDat,soilPropDat,by=c('siteID','year'))
print("loaded soilProp")

#NEON Single Air Temp at Various Heights ----
dataName="NEON_SingleAirTemperature"
funName="mean"
rm(phenoDat,soilPropDat)
allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                         dataName=dataName,dataPath=dataPath,funName=funName,baseTemp=baseTemp)
allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
rm(allWeekDatList)

tempDat <- pivot_wider(allWeeks,names_from=verticalPosition,values_from=c(GDD,CDD))
rm(allWeeks)

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

#NEON Soil Moisture ----
dataName="NEON_SoilMoisture"
funName="mean"
allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                         dataName=dataName,dataPath=dataPath,funName=funName)
allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
rm(allWeekDatList)

soilMoistureDat <- pivot_wider(allWeeks,names_from=verticalPosition,values_from=soil_moisture)
rm(allWeeks)

allComDat <- left_join(allComDat,soilMoistureDat,by=c('siteID','date'))
rm(soilMoistureDat)

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
allComDat <- allComDat %>% dplyr::select(-c(soilTemp_,soilTemp_O,
                                            soilTemp_M,litterDepth_,soilMoisture_,
                                            newScientificName,
                                            litterDepth_O,litterDepth_M,soilMoisture_O,soilMoisture_M,
                                            dryMassFraction_O,dryMassFraction_M,dryMassFraction_))
allComDat <- allComDat %>% filter(year<2022) %>% 
  filter(!is.na(CDD_10))

allComDat$month <- lubridate::month(as.Date(allComDat$dayOfYear,
                                            origin=paste0((allComDat$year-1),"-12-31")))
allComDat$dy <- lubridate::day(as.Date(allComDat$dayOfYear,
                                       origin=paste0((allComDat$year-1),"-12-31")))

#Add Drought ----
combinedDrought <- read.csv(file="droughtsAtNEON.csv")

allComDat <- left_join(allComDat,combinedDrought,by=c("siteID","year"))

allComDat$drought <- NA
allComDat$drought[allComDat$month==1] <- allComDat$X1[allComDat$month==1]
allComDat$drought[allComDat$month==2 &allComDat$dy<15] <- allComDat$X1[allComDat$month==2 &allComDat$dy<15]
allComDat$drought[allComDat$month==2 &allComDat$dy>14] <- allComDat$X2[allComDat$month==2 &allComDat$dy>14]

allComDat$drought[allComDat$month==3 &allComDat$dy<15] <- allComDat$X2[allComDat$month==3 &allComDat$dy<15]
allComDat$drought[allComDat$month==3 &allComDat$dy>14] <- allComDat$X3[allComDat$month==3 &allComDat$dy>14]

allComDat$drought[allComDat$month==4 &allComDat$dy<15] <- allComDat$X3[allComDat$month==4 &allComDat$dy<15]
allComDat$drought[allComDat$month==4 &allComDat$dy>14] <- allComDat$X4[allComDat$month==4 &allComDat$dy>14]

allComDat$drought[allComDat$month==5 &allComDat$dy<15] <- allComDat$X4[allComDat$month==5 &allComDat$dy<15]
allComDat$drought[allComDat$month==5 &allComDat$dy>14] <- allComDat$X5[allComDat$month==5 &allComDat$dy>14]

allComDat$drought[allComDat$month==6 &allComDat$dy<15] <- allComDat$X5[allComDat$month==6 &allComDat$dy<15]
allComDat$drought[allComDat$month==6 &allComDat$dy>14] <- allComDat$X6[allComDat$month==6 &allComDat$dy>14]

allComDat$drought[allComDat$month==7 &allComDat$dy<15] <- allComDat$X6[allComDat$month==7 &allComDat$dy<15]
allComDat$drought[allComDat$month==7 &allComDat$dy>14] <- allComDat$X7[allComDat$month==7 &allComDat$dy>14]

allComDat$drought[allComDat$month==8 &allComDat$dy<15] <- allComDat$X7[allComDat$month==8 &allComDat$dy<15]
allComDat$drought[allComDat$month==8 &allComDat$dy>14] <- allComDat$X8[allComDat$month==8 &allComDat$dy>14]

allComDat$drought[allComDat$month==9 &allComDat$dy<15] <- allComDat$X8[allComDat$month==9 &allComDat$dy<15]
allComDat$drought[allComDat$month==9 &allComDat$dy>14] <- allComDat$X9[allComDat$month==9 &allComDat$dy>14]

allComDat$drought[allComDat$month==10 &allComDat$dy<15] <- allComDat$X9[allComDat$month==10 &allComDat$dy<15]
allComDat$drought[allComDat$month==10 &allComDat$dy>14] <- allComDat$X10[allComDat$month==10 &allComDat$dy>14]

allComDat$drought[allComDat$month==11 &allComDat$dy<15] <- allComDat$X10[allComDat$month==11 &allComDat$dy<15]
allComDat$drought[allComDat$month==11 &allComDat$dy>14] <- allComDat$X11[allComDat$month==11 &allComDat$dy>14]

allComDat$drought[allComDat$month==12 &allComDat$dy<15] <- allComDat$X11[allComDat$month==12 &allComDat$dy<15]
allComDat$drought[allComDat$month==12 &allComDat$dy>14] <- allComDat$X12[allComDat$month==12 &allComDat$dy>14]

allComDat <- allComDat %>% ungroup()
allComDat$X7 <- as.numeric(allComDat$X7)
allComDat$X8 <- as.numeric(allComDat$X8)
allComDat$X9 <- as.numeric(allComDat$X9)
allComDat <- allComDat %>% mutate(averageDrought = rowMeans(dplyr::select(allComDat,X7,X8,X9),na.rm=TRUE))

#Changing Variable Types ----
allComDat$phenoStatus[allComDat$phenoStatus=="yes"] <- 1
allComDat$phenoStatus[allComDat$phenoStatus=="no"] <- 0

allComDat$phenoStatus <- as.factor(allComDat$phenoStatus)
allComDat$GDD_10 <- as.numeric(allComDat$GDD_10)
allComDat$CDD_10 <- as.numeric(allComDat$CDD_10)
allComDat$CDD_closest <- as.numeric(allComDat$CDD_closest)
allComDat$daylength <- as.numeric(allComDat$daylength)
allComDat$soilMoisture <- as.numeric(allComDat$`501`)
allComDat$drought <- as.numeric(allComDat$drought)
allComDat$Shade.tolerance <- as.numeric(allComDat$Shade.tolerance)
allComDat$Drought.tolerance <- as.numeric(allComDat$Drought.tolerance)
allComDat$height <- as.numeric(allComDat$height)
allComDat$growthForm <- as.factor(allComDat$growthForm)
allComDat$Mycorrhizal.type <- as.factor(allComDat$Mycorrhizal.type)
allComDat$canopyPosition <- as.factor(allComDat$canopyPosition)
allComDat$CNratio <- as.numeric(allComDat$CNratio)
allComDat$field_mean_annual_temperature_C <- as.numeric(allComDat$field_mean_annual_temperature_C)
allComDat$field_mean_annual_precipitation_mm <- as.numeric(allComDat$field_mean_annual_precipitation_mm)

allComDat <- allComDat %>% filter(!is.na(drought)) %>% filter(dayOfYear>=213) %>%
  filter(plantStatus=="Live")

allComDat$X1=allComDat$X2=allComDat$X3=allComDat$X4=
  allComDat$X5=allComDat$X6=allComDat$X7=allComDat$X8=
  allComDat$X9=allComDat$X10=allComDat$X11=allComDat$X12<- NULL

#Edit by Adding CDD*daylength ratio ----
allSunlightTimes <- read.csv(file=paste0(dataPath,'NEON_sunlightTimes.csv'))
siteMaxDaylength <- allSunlightTimes %>% group_by(siteID) %>% 
  summarize(maxDayLength=max(daylength))
allComDat <- left_join(allComDat,siteMaxDaylength,by="siteID") %>%
  mutate(pRatio=daylength/maxDayLength)

allComDat$CDDp <- allComDat$CDD_closest * allComDat$pRatio

save(allComDat,file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData"))
