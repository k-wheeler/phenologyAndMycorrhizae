#Determine CDD basetemps for each site in non-drought years
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')
#library(data.table)
source('computeWeeklyMetData.R')
library('purrr')

#Load and Combine Data----
#NEON Phenology Observations (and vegetation structural characteristics and mycorrhizae association) ----

p=3 #For colored leaves
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
#phenoDat <- phenoDat %>% filter((date.x <= date.y),!is.na(height))
phenoDat <- phenoDat %>% filter((date.x <= date.y & date.x >= (as.Date(date.y)-14)),!is.na(height))
names(phenoDat)[names(phenoDat)=="date.x"] <- "date"

phenoDat <- phenoDat %>% filter(growthForm=="Deciduous broadleaf") %>%
  dplyr::select(siteID,date,year,phenoStatus,height)

rm(subPhenoDat)

print("loaded PhenoDat")

#Load Temperature ----
dataName="NEON_SingleAirTemperature"
funName="mean"

for(baseTemp in seq(0,20,5)){
  #baseTemp <- 0
  print(baseTemp)
  allWeekDatList <- lapply(X=NEON_siteNames,FUN=readTotalMetDataFiles,p=p,
                           dataName=dataName,dataPath=dataPath,funName=funName,baseTemp=baseTemp)
  allWeeks <-rbindlist(allWeekDatList,fill=TRUE)
  rm(allWeekDatList)
  
  tempDat <- pivot_wider(allWeeks,names_from=verticalPosition,values_from=c(GDD,CDD))
  rm(allWeeks)
  
  phenoDat <- left_join(phenoDat,tempDat,by=c('siteID','date'))
  
  metList=apply(X=phenoDat,MARGIN=1,FUN=determineVerticalValue_closest) #Calculates for each row 
  
  if(is.null(nrow(metList))){
    metList_unlisted <- data.frame(t(sapply(metList, function(x) x[1:2])))
    metList_unlisted <- t(metList_unlisted)
    metList <- metList_unlisted
    rm(metList_unlisted)
  }
  phenoDat$CDD_closest <- metList[1,]
  names(phenoDat)[names(phenoDat)=="CDD_closest"] <- paste0("CDD_base_",baseTemp)
  phenoDat$CDD_10=phenoDat$CDD_20=phenoDat$CDD_30=phenoDat$CDD_40=phenoDat$CDD_50=
    phenoDat$GDD_10=phenoDat$GDD_20=phenoDat$GDD_30=phenoDat$GDD_40=phenoDat$GDD_50=NULL
  
  rm(tempDat)
}

print("Finished joining temp")

#Load and Combine with Drought ----
combinedDrought <- read.csv(file="droughtsAtNEON.csv")

selectAllComDat <- left_join(phenoDat,combinedDrought,by=c("siteID","year"))
selectAllComDat$month <- lubridate::month(selectAllComDat$date)
selectAllComDat$dy <- lubridate::day(selectAllComDat$date)

selectAllComDat$drought <- NA
selectAllComDat$drought[selectAllComDat$month==1] <- selectAllComDat$X1[selectAllComDat$month==1]
selectAllComDat$drought[selectAllComDat$month==2 &selectAllComDat$dy<15] <- selectAllComDat$X1[selectAllComDat$month==2 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==2 &selectAllComDat$dy>14] <- selectAllComDat$X2[selectAllComDat$month==2 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==3 &selectAllComDat$dy<15] <- selectAllComDat$X2[selectAllComDat$month==3 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==3 &selectAllComDat$dy>14] <- selectAllComDat$X3[selectAllComDat$month==3 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==4 &selectAllComDat$dy<15] <- selectAllComDat$X3[selectAllComDat$month==4 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==4 &selectAllComDat$dy>14] <- selectAllComDat$X4[selectAllComDat$month==4 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==5 &selectAllComDat$dy<15] <- selectAllComDat$X4[selectAllComDat$month==5 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==5 &selectAllComDat$dy>14] <- selectAllComDat$X5[selectAllComDat$month==5 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==6 &selectAllComDat$dy<15] <- selectAllComDat$X5[selectAllComDat$month==6 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==6 &selectAllComDat$dy>14] <- selectAllComDat$X6[selectAllComDat$month==6 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==7 &selectAllComDat$dy<15] <- selectAllComDat$X6[selectAllComDat$month==7 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==7 &selectAllComDat$dy>14] <- selectAllComDat$X7[selectAllComDat$month==7 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==8 &selectAllComDat$dy<15] <- selectAllComDat$X7[selectAllComDat$month==8 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==8 &selectAllComDat$dy>14] <- selectAllComDat$X8[selectAllComDat$month==8 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==9 &selectAllComDat$dy<15] <- selectAllComDat$X8[selectAllComDat$month==9 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==9 &selectAllComDat$dy>14] <- selectAllComDat$X9[selectAllComDat$month==9 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==10 &selectAllComDat$dy<15] <- selectAllComDat$X9[selectAllComDat$month==10 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==10 &selectAllComDat$dy>14] <- selectAllComDat$X10[selectAllComDat$month==10 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==11 &selectAllComDat$dy<15] <- selectAllComDat$X10[selectAllComDat$month==11 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==11 &selectAllComDat$dy>14] <- selectAllComDat$X11[selectAllComDat$month==11 &selectAllComDat$dy>14]

selectAllComDat$drought[selectAllComDat$month==12 &selectAllComDat$dy<15] <- selectAllComDat$X11[selectAllComDat$month==12 &selectAllComDat$dy<15]
selectAllComDat$drought[selectAllComDat$month==12 &selectAllComDat$dy>14] <- selectAllComDat$X12[selectAllComDat$month==12 &selectAllComDat$dy>14]

selectAllComDat <- selectAllComDat %>% mutate(averageDrought = rowMeans(dplyr::select(selectAllComDat,X7,X8,X9),na.rm=TRUE))

selectAllComDat$X1=selectAllComDat$X2=selectAllComDat$X3=selectAllComDat$X4=
  selectAllComDat$X5=selectAllComDat$X6=selectAllComDat$X7=selectAllComDat$X8=
  selectAllComDat$X9=selectAllComDat$X10=selectAllComDat$X11=selectAllComDat$X12 <- NULL

selectAllComDat <- selectAllComDat %>% filter(drought>0)
selectSites <- unique(selectAllComDat$siteID)
pVals <- matrix(nrow=length(selectSites),ncol=7)
for(s in seq_along(selectSites)){
  print(selectSites[s])
  siteDat <- selectAllComDat %>% filter(siteID==selectSites[s])
  yesDat <- siteDat %>% filter(phenoStatus=="yes")
  noDat <- siteDat %>% filter(phenoStatus=="no")
  print(nrow(yesDat))
  print(nrow(noDat))
  
  pVals[s,1] <- selectSites[s]
  pVals[s,2] <- t.test(yesDat$CDD_base_0,noDat$CDD_base_0)$p.value
  pVals[s,3] <- t.test(yesDat$CDD_base_5,noDat$CDD_base_5)$p.value
  pVals[s,4] <- t.test(yesDat$CDD_base_10,noDat$CDD_base_10)$p.value
  pVals[s,5] <- t.test(yesDat$CDD_base_15,noDat$CDD_base_15)$p.value
  pVals[s,6] <- t.test(yesDat$CDD_base_20,noDat$CDD_base_20)$p.value
  pVals[s,7] <- which.min(c(as.numeric(pVals[s,2]),as.numeric(pVals[s,3]),
                    as.numeric(pVals[s,4]),as.numeric(pVals[s,5]),as.numeric(pVals[s,6])))
}



#Next steps editing: Don't include all of the no's. Just the ones right before the yes.
##Currently they all are showing significant differences
