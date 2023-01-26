# args = commandArgs(trailingOnly=TRUE)
# print(args)
# my_task_id=as.numeric(args[1])
# num_tasks=as.numeric(args[2])
# fnames <- 10:47
# my_fnames=fnames[seq((my_task_id+1),47,num_tasks)]
# 
# for(s in my_fnames){

source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

library('broom')

tempDat <- read.csv('Data/NEON_SingleAirTemperatureALLdata.csv')
tempDat <- tempDat %>% filter(finalQF==0) %>% filter(!is.na(tempSingleMean))
fittedModelDat <- tempDat %>% group_by(siteID,startDateTime) %>% mutate(n=n()) %>% filter(n>1)

fittedModels <- fittedModelDat %>% group_by(siteID,startDateTime) %>% 
  do(model = lm(tempSingleMean ~ verticalPosition,data=.))

fittedModelsTidied <- fittedModels %>% summarise(tidy(model))
fittedModelsFit <- fittedModels %>% summarise(glance(model))
fittedModels <- cbind(fittedModels,fittedModelsFit$r.squared)

newDat <- matrix(nrow=nrow(fittedModelsTidied),ncol=3)

ct <- 1
for(i in 1:nrow(fittedModels)){
  if(i%%1000==0){
    print(i)
  }
  newDat[ct,1] <- as.character(fittedModels[i,1])
  newDat[ct,2] <- as.character(fittedModels[i,2])
  newDat[ct,3] <- as.character(fittedModels[i,4])
  ct <- ct + 1
  newDat[ct,1] <- as.character(fittedModels[i,1])
  newDat[ct,2] <- as.character(fittedModels[i,2])
  newDat[ct,3] <- as.character(fittedModels[i,4])
  ct <- ct + 1
}
colnames(newDat) <- c('siteID','startDateTime','r.squared')
fittedModelSummary <- cbind(newDat,fittedModelsTidied)

save(fittedModelSummary,file="fittedAirTempModels.RData")

# fitted_models %>% tidy(model)
# fitted_models %>% augment(model)

# dataName="NEON_Roots"
# NEON_ID='DP1.10067.001'
# 
# inFileName <- 'bbc_dilution.csv'
# selectColumns <- c('plotID','sampleVolume',
#                    'dryMass','somDryMass')
# 
# dat1 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
#                         selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)
# 
# inFileName <- 'bbc_percore.csv'
# selectColumns <- c('plotID','decimalLatitude','decimalLongitude',
#                    'litterDepth','rootSampleDepth')
# 
# dat2 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
#                         selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)
# 
# allDat <- full_join(dat1,dat2,by=c('siteID','collectDate','plotID'))
# 
# inFileName <- 'bbc_rootChemistry.csv'
# selectColumns <- c('plotID','plotType',
#                    'd15N','d13C','nitrogenPercent','carbonPercent',
#                    'CNratio','cnIsotopeQF','cnPercentQF')
# 
# dat3 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
#                         selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)
# 
# allDat <- full_join(allDat,dat3,by=c('siteID','collectDate','plotID'))
# 
# inFileName <- "bbc_rootmass.csv"
# selectColumns <- c('plotID','dryMass','mycorrhizaeVisible')
# 
# dat4 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
#                         selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)
# 
# allDat <- full_join(allDat,dat4,by=c('siteID','collectDate','plotID'))
# outFileName <- paste0(dataName,"ALLdata.csv")
# write.csv(file=paste0(dataPath,outFileName),allDat,row.names = FALSE,quote=FALSE)
