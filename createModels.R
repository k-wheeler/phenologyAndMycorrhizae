p=2
load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat

library(caTools)
library('xgboost')

set.seed(0)
specificCol <- "phenoStatus"
selectAllComDat <- allComDat # %>% filter(growthForm=="Deciduous broadleaf")
# selectAllComDat <- allComDat %>% dplyr::select(siteID,year,individualID,canopyPosition,
#                                                plantStatus,stemDiameter,maxCanopyDiameter,
#                                                height,scientificName,growthForm,Mycorrhizal.type,
#                                                litterDepth_O,litterDepth_M,soilInWaterpH_O,
#                                                soilInWaterpH_M,soilMoisture_O,soilMoisture_M,
#                                                d15N,organicd13C,nitrogenPercent,organicCPercent,
#                                                CNratio,Shade.tolerance,Drought.tolerance,
#                                                Waterlogging.tolerance,phenoStatus)
selectAllComDat$phenoStatus[selectAllComDat$phenoStatus=="yes"] <- 1
selectAllComDat$phenoStatus[selectAllComDat$phenoStatus=="no"] <- 0

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
library('mgcv')

# mdl_gam <- gam(phenoStatus ~ s(GDD_10) + s(daylength),data=selectAllComDat, family = binomial(link="logit"),
#                method = "REML")
# 
# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter),data=selectAllComDat,
#                method="REML", family = binomial(link="logit")) 

# mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter) +
#                  s(soil_GDD_508,by=stemDiameter)+s(GDD_closest,by=stemDiameter)
                 
# select2 <- selectAllComDat %>% filter(!is.na(Shade.tolerance))
selectAllComDat$phenoStatus <- as.factor(selectAllComDat$phenoStatus)

selectAllComDat$GDD_10 <- as.numeric(selectAllComDat$GDD_10)
selectAllComDat$daylength <- as.numeric(selectAllComDat$daylength)
selectAllComDat$sumPrecip <- as.numeric(selectAllComDat$sumPrecip)
selectAllComDat$soil_GDD_508 <- as.numeric(selectAllComDat$soil_GDD_508)

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

mdl_gam <- gam(phenoStatus ~ s(GDD_10,by=stemDiameter) + s(daylength,by=stemDiameter) + s(sumPrecip,by=stemDiameter) +
                 s(soil_GDD_508,by=stemDiameter)+
                 s(GDD_10,by=Shade.tolerance) + s(daylength,by=Shade.tolerance) + s(sumPrecip,by=Shade.tolerance) +
                 s(soil_GDD_508,by=Shade.tolerance) +
                 s(GDD_10,by=Drought.tolerance) + s(daylength,by=Drought.tolerance) + s(sumPrecip,by=Drought.tolerance) +
                 s(soil_GDD_508,by=Drought.tolerance)+
                 s(GDD_10,by=height) + s(daylength,by=height) + s(sumPrecip,by=height) +
                 s(soil_GDD_508,by=height) +
                 s(GDD_10,by=growthForm) + s(daylength,by=growthForm) + s(sumPrecip,by=growthForm) +
                 s(soil_GDD_508,by=growthForm) +
                 s(GDD_10,by=Mycorrhizal.type) + s(daylength,by=Mycorrhizal.type) + s(sumPrecip,by=Mycorrhizal.type) +
                 s(soil_GDD_508,by=Mycorrhizal.type)+
                 s(GDD_10,by=canopyPosition) + s(daylength,by=canopyPosition) + s(sumPrecip,by=canopyPosition) +
                 s(soil_GDD_508,by=canopyPosition)+
                 s(GDD_10,by=CNratio) + s(daylength,by=CNratio) + s(sumPrecip,by=CNratio) +
                 s(soil_GDD_508,by=CNratio)+
                 s(GDD_10,by=field_mean_annual_temperature_C) + s(daylength,by=field_mean_annual_temperature_C) + 
                 s(sumPrecip,by=field_mean_annual_temperature_C) +
                 s(soil_GDD_508,by=field_mean_annual_temperature_C)+
                 s(GDD_10,by=field_mean_annual_precipitation_mm) + s(daylength,by=field_mean_annual_precipitation_mm) + 
                 s(sumPrecip,by=field_mean_annual_precipitation_mm) +
                 s(soil_GDD_508,by=field_mean_annual_precipitation_mm)
,data=selectAllComDat,family = binomial(link="logit"),method="REML")

save(mdl_gam,"mdl_gam.RData")
 
# gam.check(mdl_gam,page=1)
# plot.gam(mdl_gam,residuals=TRUE,pch=20,cex=5,shade=TRUE,page=1,all.terms = TRUE)
# gam.check(mdl_gam1,page=1)