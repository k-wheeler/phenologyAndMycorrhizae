#Drought Differences on CDD Needed for Senescence
source('sharedVariables.R')
library(caTools)
library('xgboost')
library(randomForest)
library('mgcv')
options(stringsAsFactors = FALSE)

set.seed(0)

#Load Data and General Prep ----
p=3
load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat

subDat <- allComDat %>% filter(phenoStatus==1) %>%
  dplyr::select(siteID,year,daylength,individualID,scientificName,Mycorrhizal.type,CDD_closest,soilMoisture,height,canopyPosition)

#Basic T-Tests ----
subDat2 <- subDat %>% group_by(individualID,year,height) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-6,-2,0,8))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin) %>% 
  summarize(meanCDD=mean(CDD_closest),n=n())

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanCDD)
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID",'DroughtYear',"Good","Exclude","NA")
# names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
# names(subDat3)[names(subDat3)=="(-1,4]"] <- "Good"
# names(subDat3)[names(subDat3)=="(-2,-1]"] <- "Exclude"

names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
names(subDat3)[names(subDat3)=="(0,8]"] <- "Good"
names(subDat3)[names(subDat3)=="(-2,0]"] <- "Exclude"
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID","Exclude","DroughtYear","Good","NA")

subDat4 <- subDat3 %>% summarize(droughtDiffCDD=Good-DroughtYear) %>% filter(!is.na(droughtDiffCDD))
droughtDiff_AM <- subDat4 %>% filter(Mycorrhizal.type=="AM")
droughtDiff_AM <- droughtDiff_AM$droughtDiffCDD
droughtDiff_EcM <- subDat4 %>% filter(Mycorrhizal.type=="EcM")
droughtDiff_EcM <- droughtDiff_EcM$droughtDiffCDD
length(droughtDiff_AM)
length(droughtDiff_EcM)
mean(droughtDiff_AM)
mean(droughtDiff_EcM)

print(sum(droughtDiff_AM>0)/length(droughtDiff_AM))
print(sum(droughtDiff_EcM>0)/length(droughtDiff_EcM))
t.test(droughtDiff_AM,droughtDiff_EcM)

#CDD vs Soil Moisture ----
pdf(file=paste0("CDDvsSoilMoisture.pdf"),
    width=6,height=6)
plot(subDat$soilMoisture,subDat$CDD_closest,pch=20)
points(subDat$soilMoisture[subDat$Mycorrhizal.type=="AM"],
       subDat$CDD_closest[subDat$Mycorrhizal.type=="AM"],pch=20,col="brown")
points(subDat$soilMoisture[subDat$Mycorrhizal.type=="EcM"],
       subDat$CDD_closest[subDat$Mycorrhizal.type=="EcM"],pch=20,col="cyan")

dev.off()

#Basic T-test for affect of drought on changing leaf age at senescence ----
subDat <- allComDat %>% filter(phenoStatus==1) %>%
  dplyr::select(siteID,year,daylength,individualID,scientificName,Mycorrhizal.type,
                drought,soilMoisture,height,canopyPosition,springDayOfYear,dayOfYear) %>%
  mutate(leafAge=dayOfYear-springDayOfYear)

test <- subDat %>% filter(siteID=="HARV")
pdf(file=paste0("dayOfYearVsSpringDayOfYear.pdf"),
    width=6,height=6)
plot(subDat$springDayOfYear,subDat$dayOfYear,pch=20)
points(subDat$springDayOfYear[subDat$Mycorrhizal.type=="AM"],subDat$dayOfYear[subDat$Mycorrhizal.type=="AM"],pch=20,col="brown")
points(subDat$springDayOfYear[subDat$Mycorrhizal.type=="EcM"],subDat$dayOfYear[subDat$Mycorrhizal.type=="EcM"],pch=20,col="cyan")
mdl <- lm(dayOfYear ~ springDayOfYear,data=subDat)
abline(mdl,col="red",lwd=3)

plot(test$springDayOfYear,test$dayOfYear,pch=20)
mdl <- lm(dayOfYear ~ springDayOfYear,data=test)
points(test$springDayOfYear[test$Mycorrhizal.type=="AM"],test$dayOfYear[test$Mycorrhizal.type=="AM"],pch=20,col="brown")
points(test$springDayOfYear[test$Mycorrhizal.type=="EcM"],test$dayOfYear[test$Mycorrhizal.type=="EcM"],pch=20,col="cyan")
abline(mdl,col="red",lwd=3)
mdl <- lm(dayOfYear ~ springDayOfYear,data=test[test$Mycorrhizal.type=="AM",])
abline(mdl,col="brown",lwd=3)
mdl <- lm(dayOfYear ~ springDayOfYear,data=test[test$Mycorrhizal.type=="EcM",])
abline(mdl,col="cyan",lwd=3)
dev.off()


subDat2 <- subDat %>% group_by(individualID,year,height) %>% 
  filter(!is.na(leafAge)) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-6,-2,0,8))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin) %>% 
  summarize(meanLeafAge=mean(leafAge),n=n())

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanLeafAge)
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID",'DroughtYear',"Good","Exclude","NA")
# names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
# names(subDat3)[names(subDat3)=="(-1,4]"] <- "Good"
# names(subDat3)[names(subDat3)=="(-2,-1]"] <- "Exclude"

names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
names(subDat3)[names(subDat3)=="(0,8]"] <- "Good"
names(subDat3)[names(subDat3)=="(-2,0]"] <- "Exclude"
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID","Exclude","DroughtYear","Good","NA")

subDat4 <- subDat3 %>% summarize(droughtDiffDOY=Good-DroughtYear) %>% filter(!is.na(droughtDiffDOY))
droughtDiff_AM <- subDat4 %>% filter(Mycorrhizal.type=="AM")
droughtDiff_AM <- droughtDiff_AM$droughtDiffDOY
droughtDiff_EcM <- subDat4 %>% filter(Mycorrhizal.type=="EcM")
droughtDiff_EcM <- droughtDiff_EcM$droughtDiffDOY
length(droughtDiff_AM)
length(droughtDiff_EcM)
mean(droughtDiff_AM)
mean(droughtDiff_EcM)

print(sum(droughtDiff_AM>0)/length(droughtDiff_AM))
print(sum(droughtDiff_EcM>0)/length(droughtDiff_EcM))
t.test(droughtDiff_AM,droughtDiff_EcM)

#Drought tolerance vs change ----
subDat <- allComDat %>% filter(phenoStatus==1) %>%
  dplyr::select(siteID,year,daylength,individualID,scientificName,Mycorrhizal.type,
                drought,soilMoisture,height,canopyPosition,springDayOfYear,dayOfYear,Drought.tolerance,CDD_closest) %>%
  mutate(leafAge=dayOfYear-springDayOfYear)

subDat2 <- subDat %>% group_by(individualID,year,height,Drought.tolerance,scientificName) %>% 
  filter(!is.na(leafAge)) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-6,-2,0,8))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin,Drought.tolerance,scientificName) %>% 
  summarize(meanLeafAge=mean(leafAge),n=n())

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanLeafAge)

names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
names(subDat3)[names(subDat3)=="(0,8]"] <- "Good"
names(subDat3)[names(subDat3)=="(-2,0]"] <- "Exclude"

subDat4 <- subDat3 %>% mutate(droughtDiffDOY=Good-DroughtYear) %>% filter(!is.na(droughtDiffDOY))

pdf(file=paste0("ChangeLeafAgeVsDroughtTolerance.pdf"),
    width=6,height=6)
plot(subDat4$Drought.tolerance,subDat4$droughtDiffDOY,pch=20,main="LeafAge")
points(subDat4$Drought.tolerance[subDat4$Mycorrhizal.type=="AM"],
       subDat4$droughtDiffDOY[subDat4$Mycorrhizal.type=="AM"],pch=20,col="brown")
points(subDat4$Drought.tolerance[subDat4$Mycorrhizal.type=="EcM"],
       subDat4$droughtDiffDOY[subDat4$Mycorrhizal.type=="EcM"],pch=20,col="cyan")
mdl <- lm(droughtDiffDOY~Drought.tolerance,data=subDat4)
abline(mdl,col="red")
print(summary(mdl))

subDat2 <- subDat %>% group_by(individualID,year,height,Drought.tolerance,scientificName) %>% 
  filter(!is.na(CDD_closest)) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-6,-2,0,8))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin,Drought.tolerance,scientificName) %>% 
  summarize(meanCDD=mean(CDD_closest),n=n())

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanCDD)

names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
names(subDat3)[names(subDat3)=="(0,8]"] <- "Good"
names(subDat3)[names(subDat3)=="(-2,0]"] <- "Exclude"

subDat4 <- subDat3 %>% mutate(droughtDiffCDD=Good-DroughtYear) %>% filter(!is.na(droughtDiffCDD))

plot(subDat4$Drought.tolerance,subDat4$droughtDiffCDD,pch=20,main="CDD")
points(subDat4$Drought.tolerance[subDat4$Mycorrhizal.type=="AM"],
       subDat4$droughtDiffCDD[subDat4$Mycorrhizal.type=="AM"],pch=20,col="brown")
points(subDat4$Drought.tolerance[subDat4$Mycorrhizal.type=="EcM"],
       subDat4$droughtDiffCDD[subDat4$Mycorrhizal.type=="EcM"],pch=20,col="cyan")
mdl <- lm(droughtDiffCDD~Drought.tolerance,data=subDat4)
abline(mdl,col="red")
print(summary(mdl))

dev.off()

droughtDiff_AM <- subDat4 %>% filter(Mycorrhizal.type=="AM")
droughtDiff_AM <- droughtDiff_AM$droughtDiffDOY
droughtDiff_EcM <- subDat4 %>% filter(Mycorrhizal.type=="EcM")
droughtDiff_EcM <- droughtDiff_EcM$droughtDiffDOY
length(droughtDiff_AM)
length(droughtDiff_EcM)
mean(droughtDiff_AM)
mean(droughtDiff_EcM)

print(sum(droughtDiff_AM>0)/length(droughtDiff_AM))
print(sum(droughtDiff_EcM>0)/length(droughtDiff_EcM))
t.test(droughtDiff_AM,droughtDiff_EcM)



#Random Forest Model for leafAge ----
specificCol <- "leafAge"
subDat <- allComDat %>% filter(phenoStatus==1) %>%
  dplyr::select(siteID,year,daylength,individualID,scientificName,Mycorrhizal.type,
                drought,CDD_closest,soilMoisture,height,canopyPosition,springDayOfYear,dayOfYear) %>%
  mutate(leafAge=dayOfYear-springDayOfYear)

for(MT in c("EcM","AM")){
  print(MT)

  subSelectAllComDat <- subDat %>% filter(Mycorrhizal.type==MT) %>%
    dplyr::select(leafAge,siteID,soilMoisture,scientificName,CDD_closest) %>%
    filter(!is.na(leafAge),!is.na(soilMoisture),!is.na(scientificName),!is.na(CDD_closest))
  
  subSelectAllComDat <- subSelectAllComDat[sample(nrow(subSelectAllComDat),30000), ]
  rownames(subSelectAllComDat) <- NULL
  j <- which(names(subSelectAllComDat)==specificCol)
  
  sample = sample.split(seq(1,30000), SplitRatio = 2/3)
  train = subset(subSelectAllComDat, sample == TRUE)
  
  rf <- randomForest(leafAge ~ ., data=train)
  test  = subset(subSelectAllComDat, sample == FALSE)
  train_x <- data.frame(train[,-j])
  train_y <- data.frame(train[,j])
  test_x <- data.frame(test[,-j])
  test_y <- as.vector(test$leafAge)
  pred_y <- predict(rf,test_x)
  
  mdl <- lm(pred_y~test_y)
  print(summary(mdl))
  
  #Partial Plot ----
  pdf(file=paste0("randomForestModel_partialPlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM_leafAge.pdf"),
      width=6,height=20)
  partialPlot(x=rf,pred.data=data.frame(train),x.var="soilMoisture")
  dev.off()
  
  #Importance Plot ----
  pdf(file=paste0("randomForestModel_ImportancePlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM_leafAge.pdf"),
      width=6,height=20)
  randomForest::varImpPlot(rf)
  dev.off()
  
  #save(rf,file=paste0("randomForestModel_",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM.RData"))
}


#Random Forest Model for CDD_closest ----
specificCol <- "CDD_closest"
for(MT in c("EcM","AM")){
  print(MT)
  # subSelectAllComDat <- subDat %>% filter(Mycorrhizal.type==MT) %>%
  #   dplyr::select(CDD_closest,siteID,individualID,soilMoisture,scientificName,height,canopyPosition,daylength) %>%
  #   filter(!is.na(CDD_closest),!is.na(soilMoisture),!is.na(scientificName))
  
  subSelectAllComDat <- subDat %>% filter(Mycorrhizal.type==MT) %>%
    dplyr::select(CDD_closest,siteID,soilMoisture,scientificName,leafAge) %>%
    filter(!is.na(CDD_closest),!is.na(soilMoisture),!is.na(scientificName),!is.na(leafAge))
  
  subSelectAllComDat <- subSelectAllComDat[sample(nrow(subSelectAllComDat),30000), ]
  rownames(subSelectAllComDat) <- NULL
  j <- which(names(subSelectAllComDat)==specificCol)

  sample = sample.split(seq(1,30000), SplitRatio = 2/3)
  train = subset(subSelectAllComDat, sample == TRUE)
  
  rf <- randomForest(CDD_closest ~ ., data=train)
  test  = subset(subSelectAllComDat, sample == FALSE)
  train_x <- data.frame(train[,-j])
  train_y <- data.frame(train[,j])
  test_x <- data.frame(test[,-j])
  test_y <- as.vector(test$CDD_closest)
  pred_y <- predict(rf,test_x)
  
  mdl <- lm(pred_y~test_y)
  print(summary(mdl))
  
  #Partial Plot ----
  pdf(file=paste0("randomForestModel_partialPlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM3.pdf"),
      width=6,height=20)
  partialPlot(x=rf,pred.data=data.frame(train),x.var="soilMoisture")
  dev.off()
  
  #Importance Plot ----
  pdf(file=paste0("randomForestModel_ImportancePlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM3.pdf"),
      width=6,height=20)
  randomForest::varImpPlot(rf)
  dev.off()
  
  #save(rf,file=paste0("randomForestModel_",MT,"_",gsub(" ","",NEON_phenophase_names[p]),"_SM.RData"))
}

for(MT in c("EcM","AM")){
  subSelectAllComDat <- subDat %>% filter(Mycorrhizal.type==MT) %>%
    dplyr::select(CDD_closest,siteID,individualID,drought,scientificName,height,canopyPosition) %>%
    filter(!is.na(CDD_closest),!is.na(drought),!is.na(scientificName))

  subSelectAllComDat <- subSelectAllComDat[sample(nrow(subSelectAllComDat),50000), ]
  rownames(subSelectAllComDat) <- NULL
  j <- which(names(subSelectAllComDat)==specificCol)
  
  sample = sample.split(seq(1,50000), SplitRatio = 2/3)
  train = subset(subSelectAllComDat, sample == TRUE)
  load(paste0("randomForestModel_",MT,"_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #loaded as rf
  

  
}
