source('sharedVariables.R')
library(caTools)
library('xgboost')
library(randomForest)
library('mgcv')
options(stringsAsFactors = FALSE)

#Load Data and General Prep ----
p=3
load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat
combinedDrought <- read.csv(file="droughtsAtNEON.csv")

set.seed(0)
specificCol <- "phenoStatus"
selectAllComDat <- allComDat %>% filter(growthForm=="Deciduous broadleaf")
selectAllComDat <- left_join(selectAllComDat,combinedDrought,by=c("siteID","year"))
selectAllComDat$month <- lubridate::month(as.Date(selectAllComDat$dayOfYear,
                           origin=paste0((selectAllComDat$year-1),"-12-31")))
selectAllComDat$dy <- lubridate::day(as.Date(selectAllComDat$dayOfYear,
                                                  origin=paste0((selectAllComDat$year-1),"-12-31")))

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

selectAllComDat <- selectAllComDat %>% mutate(averageDrought = rowMeans(dplyr::select(selectAllComDat,X8,X9,X10),na.rm=TRUE))

selectAllComDat$phenoStatus[selectAllComDat$phenoStatus=="yes"] <- 1
selectAllComDat$phenoStatus[selectAllComDat$phenoStatus=="no"] <- 0

selectAllComDat$phenoStatus <- as.factor(selectAllComDat$phenoStatus)

selectAllComDat$GDD_10 <- as.numeric(selectAllComDat$GDD_10)
selectAllComDat$CDD_10 <- as.numeric(selectAllComDat$CDD_10)
selectAllComDat$daylength <- as.numeric(selectAllComDat$daylength)
selectAllComDat$sumPrecip <- as.numeric(selectAllComDat$sumPrecip)
selectAllComDat$soil_GDD_508 <- as.numeric(selectAllComDat$soil_GDD_508)
selectAllComDat$soil_CDD_508 <- as.numeric(selectAllComDat$soil_CDD_508)
selectAllComDat$drought <- as.numeric(selectAllComDat$drought)
selectAllComDat$averageDrought <- as.numeric(selectAllComDat$averageDrought)

selectAllComDat$Shade.tolerance <- as.numeric(selectAllComDat$Shade.tolerance)
selectAllComDat$stemDiameter <- as.numeric(selectAllComDat$stemDiameter)
selectAllComDat$Drought.tolerance <- as.numeric(selectAllComDat$Drought.tolerance)
selectAllComDat$height <- as.numeric(selectAllComDat$height)
selectAllComDat$growthForm <- as.factor(selectAllComDat$growthForm)
selectAllComDat$Mycorrhizal.type <- as.factor(selectAllComDat$Mycorrhizal.type)
selectAllComDat$canopyPosition <- as.factor(selectAllComDat$canopyPosition)
selectAllComDat$CNratio <- as.numeric(selectAllComDat$CNratio)
selectAllComDat$field_mean_annual_temperature_C <- as.numeric(selectAllComDat$field_mean_annual_temperature_C)
selectAllComDat$field_mean_annual_precipitation_mm <- as.numeric(selectAllComDat$field_mean_annual_precipitation_mm)

#Checking to see how often dr
selectAllComDat <- selectAllComDat %>% filter(!is.na(drought))
subDat <- selectAllComDat %>% filter(phenoStatus==1)
subDat <- subDat[,c('siteID','individualID','Mycorrhizal.type','drought','year')]
subDat2 <- subDat %>% unique() %>% 
  mutate(droughtBin=cut(drought,breaks=seq(-5,8))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin) %>% summarise(n=n())

pdf(file="numberIndividualsRemeasuredDroughts.pdf",
    width=6,height=20)
ggplot(subDat2,aes(n,droughtBin,fill=Mycorrhizal.type)) +
  geom_density_ridges2(scale=1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))+
  xlab("Number Of Years Per Individual")+
  ylab("PDSI")+
  scale_x_continuous(limits=c(0,8))
plot(density(selectAllComDat$drought))

subDat2 <- subDat %>% group_by(individualID,year) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-4,-2,0,7))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin) %>% summarise(n=n())
subDat2 <- pivot_wider(subDat2,names_from=droughtBin,values_from=n)
names(subDat2) <- c('siteID',"Mycorrhizal.type","individualID",'DroughtYear',"Exclude","Good","NA")

subDat3 <- subDat2 %>% filter(DroughtYear>0)

subDatAM <- subDat3 %>% filter(Mycorrhizal.type=="AM")
hist(na.omit(subDatAM$Good),main="AM")
subDatEcM <- subDat3 %>% filter(Mycorrhizal.type=="EcM")
hist(na.omit(subDatEcM$Good),main="EcM")
#ggplot(subDat2,aes(x=Good,y=DroughtYear)) + geom_point()

dev.off()

#Investigating if CDD difference needed for senescence between drought and no drought significantly differs between AM and EcM ----
subDat <- selectAllComDat %>% filter(phenoStatus==1) %>%
  dplyr::select(siteID,year,individualID,scientificName,Mycorrhizal.type,CDD_10,drought)

subDat2 <- subDat %>% group_by(individualID,year) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-6,-2,-1,2))) %>%
  group_by(siteID,Mycorrhizal.type,individualID,droughtBin) %>% summarize(meanCDD=mean(CDD_10))

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanCDD)
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID",'DroughtYear',"Good","Exclude","NA")
names(subDat3)[names(subDat3)=="(-6,-2]"] <- "DroughtYear"
names(subDat3)[names(subDat3)=="(-1,2]"] <- "Good"
names(subDat3)[names(subDat3)=="(-2,-1]"] <- "Exclude"
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID","Exclude","DroughtYear","Good","NA")

subDat4 <- subDat3 %>% summarize(droughtDiffCDD=Good-DroughtYear) %>% filter(!is.na(droughtDiffCDD))
droughtDiff_AM <- subDat4 %>% filter(Mycorrhizal.type=="AM",!is.na(droughtDiffCDD))
droughtDiff_AM <- droughtDiff_AM$droughtDiffCDD
droughtDiff_EcM <- subDat4 %>% filter(Mycorrhizal.type=="EcM",!is.na(droughtDiffCDD))
droughtDiff_EcM <- droughtDiff_EcM$droughtDiffCDD
length(droughtDiff_AM)
length(droughtDiff_EcM)
mean(droughtDiff_AM)
mean(droughtDiff_EcM)
t.test(droughtDiff_AM,droughtDiff_EcM)

yesData <- selectAllComDat %>% filter(phenoStatus=="1",siteID=="HARV")
#yesData <- selectAllComDat %>% filter(phenoStatus=="1")
pdf(file="CDDvsDrought_HARV.pdf",
    width=6,height=6)
plot(yesData$drought,(yesData$CDD_closest),pch=20,cex=1)
#points(jitter(yesData$drought[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")
points(jitter(yesData$drought[yesData$Mycorrhizal.type=="AM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="AM"]),pch=20,col="brown")
points(jitter(yesData$drought[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")

plot(yesData$drought,(yesData$CDD_closest),pch=20,cex=1)
points(jitter(yesData$drought[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")
points(jitter(yesData$drought[yesData$Mycorrhizal.type=="AM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="AM"]),pch=20,col="brown")

plot(density(na.omit(yesData$CDD_closest[yesData$Mycorrhizal.type=="AM"])),col="brown")
lines(density(na.omit(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"])),col="cyan")

plot(yesData$dayOfYear,yesData$CDD_closest,pch=20)
points(jitter(yesData$dayOfYear[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")
points(jitter(yesData$dayOfYear[yesData$Mycorrhizal.type=="AM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="AM"]),pch=20,col="brown")

plot(yesData$dayOfYear,yesData$CDD_closest,pch=20)
points(jitter(yesData$dayOfYear[yesData$Mycorrhizal.type=="AM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="AM"]),pch=20,col="brown")
points(jitter(yesData$dayOfYear[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_closest[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")

dev.off()

#Species Instead of Individuals (Not working because there are so few)
subDat2 <- subDat %>% group_by(individualID,year) %>% unique() %>%
  mutate(droughtBin=cut(drought,breaks=c(-4,-2,-1,1))) %>%
  group_by(siteID,Mycorrhizal.type,scientificName,droughtBin) %>% summarize(meanCDD=mean(CDD_10))

subDat3 <- pivot_wider(subDat2,names_from=droughtBin,values_from=meanCDD)
#names(subDat3) <- c('siteID',"Mycorrhizal.type","individualID",'DroughtYear',"Good","Exclude","NA")
names(subDat3) <- c('siteID',"Mycorrhizal.type","scientificName","Exclude","NA","DroughtYear","Good")

subDat3 <- subDat3 %>% mutate(droughtDiffCDD=Good-DroughtYear)
droughtDiff_AM <- subDat3 %>% filter(Mycorrhizal.type=="AM",!is.na(droughtDiffCDD))
droughtDiff_AM <- droughtDiff_AM$droughtDiffCDD
droughtDiff_EcM <- subDat3 %>% filter(Mycorrhizal.type=="EcM",!is.na(droughtDiffCDD))
droughtDiff_EcM <- droughtDiff_EcM$droughtDiffCDD
length(droughtDiff_AM)
length(droughtDiff_EcM)
mean(droughtDiff_AM)
mean(droughtDiff_EcM)


#Cooling Driver Machine Learning ----
rm(allComDat)
MT <- "EcM"
specificCol <- "CDD_10"
subSelectAllComDat <- selectAllComDat %>% filter(Mycorrhizal.type==MT) %>%
  dplyr::select(CDD_10,daylength,siteID,individualID,drought) %>%
  filter(!is.na(CDD_10),!is.na(daylength),!is.na(siteID),!is.na(individualID),
         !is.na(drought))

subSelectAllComDat <- subSelectAllComDat[sample(nrow(subSelectAllComDat),300000), ]
rownames(subSelectAllComDat) <- NULL
j <- which(names(subSelectAllComDat)==specificCol)
if(MT=="AM"){
subSelectAllComDat <- subSelectAllComDat[sample(nrow(subSelectAllComDat),300000), ]
}
sample = sample.split(subSelectAllComDat[,j], SplitRatio = .75)
train = subset(subSelectAllComDat, sample == TRUE) #4445704       6

rf <- randomForest(CDD_10 ~ ., data=train)
save(rf,file=paste0("randomForestModel_",MT,"_",gsub(" ","",NEON_phenophase_names[p]),".RData"))

test  = subset(subSelectAllComDat, sample == FALSE)
train_x <- data.matrix(train[,-j])
train_y <- data.matrix(train[,j])

test_x <- data.matrix(test[,-j])
test_y <- data.matrix(test[,j])

#R^2 ----
pred_y <- predict(rf,test_x)
mdl <- lm(pred_y~test_y)
summary(mdl)

#Partial Plot ----
pdf(file=paste0("randomForestModel_partialPlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),".pdf"),
    width=6,height=20)
partialPlot(x=rf,pred.data=train,x.var="drought")
dev.off()

#Importance Plot ----
pdf(file=paste0("randomForestModel_ImportancePlot",MT,"_",gsub(" ","",NEON_phenophase_names[p]),".pdf"),
    width=6,height=20)
randomForest::varImpPlot(rf)
dev.off()

#rf <- randomForest(phenoStatus ~ ., data=train)
rf <- randomForest(CDD_10 ~ ., data=train)
pred_y <- predict(rf,test_x)
#sum(pred_y==test_y)/length(test_y) #Percentage of correct predictions 

mdl <- lm(pred_y~test_y)
summary(mdl)

pdf(file="partialDependencePlot_EcM_GDD.pdf",
    width=6,height=20)
partialPlot(x=rf,pred.data=train,x.var="drought")
dev.off()


selectAllComDat_EcM <- selectAllComDat %>% filter(Mycorrhizal.type=="EcM") %>%
  dplyr::select(CDD_10,daylength,siteID,individualID,drought,phenoStatus) %>%
  filter(!is.na(CDD_10),!is.na(daylength),!is.na(siteID),!is.na(individualID),
         !is.na(drought),!is.na(phenoStatus))



#Warming Driver ----
rm(allComDat)
specificCol <- "GDD_10"
MT <- "AM"
subSelectAllComDat <- selectAllComDat %>% filter(Mycorrhizal.type==MT) %>%
  dplyr::select(GDD_10,daylength,siteID,individualID,drought,phenoStatus) %>%
  filter(!is.na(GDD_10),!is.na(daylength),!is.na(siteID),!is.na(individualID),
         !is.na(drought),!is.na(phenoStatus))

j <- which(names(subSelectAllComDat)==specificCol)

sample = sample.split(subSelectAllComDat[,j], SplitRatio = .75)
train = subset(subSelectAllComDat, sample == TRUE) #4445704       6
rf <- randomForest(GDD_10 ~ ., data=train)

save(rf,file=paste0("randomForestModel_",MT,"_",gsub(" ","",NEON_phenophase_names[p]),".RData"))

#Machine Learning Models ----



# selectAllComDat <- allComDat %>% dplyr::select(siteID,year,individualID,canopyPosition,
#                                                plantStatus,stemDiameter,maxCanopyDiameter,
#                                                height,scientificName,growthForm,Mycorrhizal.type,
#                                                litterDepth_O,litterDepth_M,soilInWaterpH_O,
#                                                soilInWaterpH_M,soilMoisture_O,soilMoisture_M,
#                                                d15N,organicd13C,nitrogenPercent,organicCPercent,
#                                                CNratio,Shade.tolerance,Drought.tolerance,
#                                                Waterlogging.tolerance,phenoStatus)


j <- which(names(selectAllComDat)==specificCol)

# sample = sample.split(selectAllComDat[,j], SplitRatio = .75)
# train = subset(selectAllComDat, sample == TRUE)
# test  = subset(selectAllComDat, sample == FALSE)
# train_x <- data.matrix(train[,-j])
# train_y <- data.matrix(train[,j])
# 
# test_x <- data.matrix(test[,-j])
# test_y <- data.matrix(test[,j])
# 
# xgb_train = xgb.DMatrix(data = train_x, label = train_y)
# xgb_test = xgb.DMatrix(data = test_x, label = test_y)
# 
# watchlist = list(train=xgb_train, test=xgb_test)
# 
# model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 1500)
# pred_y <- predict(model,test_x)
# mdl <- lm(pred_y~test_y)
# summary(mdl)
# 
# importance_matrix <- xgb.importance(
#   feature_names = colnames(train_x), 
#   model = model
# )
# 
# pdf(file="importancePlot.pdf",
#     width=6,height=20)
# xgb.plot.importance(importance_matrix)
# dev.off()

#GAMS


# mdl_gam <- gam(phenoStatus ~ s(GDD_10) + s(daylength),data=selectAllComDat, family = binomial(link="logit"),
#                method = "REML")
# 
# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter),data=selectAllComDat,
#                method="REML", family = binomial(link="logit")) 

# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter) +
#                  s(soil_GDD_508,by=stemDiameter)+s(GDD_closest,by=stemDiameter)
                 
# select2 <- selectAllComDat %>% filter(!is.na(Shade.tolerance))

# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter) +
#                  s(soil_GDD_508,by=stemDiameter)+
#                  s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance) +
#                  s(soil_GDD_508,by=Shade.tolerance) +
#                  s(GDD_10,by=Drought.tolerance) + s(daylength,by=Drought.tolerance) + s(sumPrecip,by=Drought.tolerance) +
#                  s(soil_GDD_508,by=Drought.tolerance)+
#                  s(GDD_10,by=height) + s(daylength,by=height) + s(sumPrecip,by=height) +
#                  s(soil_GDD_508,by=height) +
#                  s(GDD_10,by=growthForm) + s(daylength,by=growthForm) + s(sumPrecip,by=growthForm) +
#                  s(soil_GDD_508,by=growthForm) +
#                  s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(sumPrecip,by=Mycorrhizal.type) +
#                  s(soil_GDD_508,by=Mycorrhizal.type)+
#                  s(GDD_10,by=canopyPosition) + s(daylength,by=canopyPosition) + s(sumPrecip,by=canopyPosition) +
#                  s(soil_GDD_508,by=canopyPosition)+
#                  s(GDD_10,by=CNratio) + s(daylength,by=CNratio) + s(sumPrecip,by=CNratio) +
#                  s(soil_GDD_508,by=CNratio)+
#                  s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C) + 
#                  s(sumPrecip,by=field_mean_annual_temperature_C) +
#                  s(soil_GDD_508,by=field_mean_annual_temperature_C)+
#                  s(GDD_10,by=field_mean_annual_precipitation_mm) + s(daylength,by=field_mean_annual_precipitation_mm) + 
#                  s(sumPrecip,by=field_mean_annual_precipitation_mm) +
#                  s(soil_GDD_508,by=field_mean_annual_precipitation_mm)
# ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter)+
#                  s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance)+
#                  s(GDD_10,by=height) + s(daylength,by=height) + s(sumPrecip,by=height) +
#                  s(GDD_10,by=growthForm) + s(daylength,by=growthForm) + s(sumPrecip,by=growthForm) +
#                  s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(sumPrecip,by=Mycorrhizal.type)+
#                  s(GDD_10,by=CNratio) + s(daylength,by=CNratio) + s(sumPrecip,by=CNratio)+
#                  s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C) + 
#                  s(sumPrecip,by=field_mean_annual_temperature_C)
#                ,data=selectAllComDat,family = binomial(link="logit"),method="REML")


mdl_gam_drought <- gam(phenoStatus ~ s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(drought,by=Mycorrhizal.type) + siteID
                  ,data=selectAllComDat,family = binomial(link="logit"),method="REML")



mdl_gam_AM_drought <- gam(phenoStatus ~ drought
                       ,data=AMdat,family = binomial(link="logit"),method="REML")

mdl_gam_EcM_drought <- gam(phenoStatus ~ drought
                          ,data=EcMdat,family = binomial(link="logit"),method="REML")


mdl_gam_drought <- gam(phenoStatus ~ s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(drought,by=Mycorrhizal.type) + siteID
                       ,data=selectAllComDat,family = binomial(link="logit"),method="REML")


mdl_gam_sD <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter)
               ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_sT <- gam(phenoStatus ~ s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance)
                  ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_aT <- gam(phenoStatus ~ s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C)
                  + s(sumPrecip,by=field_mean_annual_temperature_C)
                  ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_m <- gam(phenoStatus ~ s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(sumPrecip,by=Mycorrhizal.type)
                  ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_CNratio <- gam(phenoStatus ~ s(GDD_10,by=CNratio) + s(daylength,by=CNratio) + s(sumPrecip,by=CNratio)
                 ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_growthForm <- gam(phenoStatus ~ s(GDD_10,by=growthForm) + s(daylength,by=growthForm) + s(sumPrecip,by=growthForm)
                       ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

mdl_gam_growthForm2 <- gam(phenoStatus ~ s(GDD_10,by=growthForm)
                          ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

subSelectAllComDat <- selectAllComDat %>% filter(!is.na(dayOfYear),!is.na(stemDiameter),
                                                 !is.na(maxCanopyDiameter),
                                                 !is.na(Shade.tolerance), !is.na(Drought.tolerance),
                                                 !is.na(height),
                                                 !is.na(growthForm),!is.na(Mycorrhizal.type),
                                                 !is.na(field_latitude),
                                                 !is.na(nitrogenPercent),!is.na(canopyPosition),
                                                 !is.na(CNratio),
                                                 !is.na(field_mean_annual_temperature_C),
                                                 !is.na(field_mean_annual_precipitation_mm),
                                                 !is.na(GDD_10),
                                                 !is.na(daylength),
                                                 !is.na(sumPrecip))

mdl_gam_combined <- gam(phenoStatus ~ s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance)
                  +s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C)
                  + s(sumPrecip,by=field_mean_annual_temperature_C)
                  ,data=subSelectAllComDat,family = binomial(link="logit"),method="REML",na.action = 'na.fail')



library(MuMIn)

dredgeOutput_gam_combined <- dredge(mdl_gam_combined,trace=2)


               +
                 s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance)+
                 s(GDD_10,by=height) + s(daylength,by=height) + s(sumPrecip,by=height) +
                 s(GDD_10,by=growthForm) + s(daylength,by=growthForm) + s(sumPrecip,by=growthForm) +
                 s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(sumPrecip,by=Mycorrhizal.type)+
                 s(GDD_10,by=CNratio) + s(daylength,by=CNratio) + s(sumPrecip,by=CNratio)+
                 s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C) +
                 s(sumPrecip,by=field_mean_annual_temperature_C)
               ,data=selectAllComDat,family = binomial(link="logit"),method="REML")

save(mdl_gam,"mdl_gam_sub.RData")
 
#LM to predict DOY from these characteristics
library(MuMIn)
subSelectAllComDat <- selectAllComDat %>% filter(!is.na(dayOfYear),!is.na(stemDiameter),
                                                 !is.na(maxCanopyDiameter),
                                                 !is.na(Shade.tolerance), !is.na(Drought.tolerance),
                                                 !is.na(height),
                                                 !is.na(growthForm),!is.na(Mycorrhizal.type),
                                                 !is.na(field_latitude),
                                                 !is.na(nitrogenPercent),!is.na(canopyPosition),
                                                 !is.na(CNratio),
                                                 !is.na(field_mean_annual_temperature_C),
                                                 !is.na(field_mean_annual_precipitation_mm),
                                                 !is.na(GDD_10))
  
mdl_lm <- lm(dayOfYear ~ stemDiameter + maxCanopyDiameter + Shade.tolerance + Drought.tolerance +
               height + growthForm + Mycorrhizal.type + field_latitude + nitrogenPercent +
               canopyPosition + CNratio + field_mean_annual_temperature_C +
               field_mean_annual_precipitation_mm,data=subSelectAllComDat,na.action='na.fail')

dredge(mdl_lm)

specificCol <- "GDD_10"

subSelectAllComDat <- subSelectAllComDat %>% dplyr::select(GDD_10,stemDiameter,
                                                 maxCanopyDiameter,
                                                 Shade.tolerance, Drought.tolerance,
                                                 height,
                                                 growthForm,Mycorrhizal.type,
                                                 field_latitude,
                                                 nitrogenPercent,canopyPosition,
                                                 CNratio,
                                                 field_mean_annual_temperature_C,
                                                 field_mean_annual_precipitation_mm)




xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 2000)
pred_y <- predict(model,test_x)
mdl <- lm(pred_y~test_y)
summary(mdl)

importance_matrix <- xgb.importance(
  feature_names = colnames(train_x),
  model = model_AM
)

pdf(file="importancePlot_EcM.pdf",
    width=6,height=20)
xgb.plot.importance(importance_matrix)
dev.off()
model_EcM=model

# gam.check(mdl_gam,page=1)
# plot.gam(mdl_gam,residuals=TRUE,pch=20,cex=5,shade=TRUE,page=1,all.terms = TRUE)
# gam.check(mdl_gam1,page=1)