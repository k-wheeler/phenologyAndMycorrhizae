source('sharedVariables.R')

s=2
for(s in seq_along(NEON_siteNames)){
  print(NEON_siteNames[s])
  dir.create(paste0(dataPath,'NEON_SingleAirTemperature/',NEON_siteNames[s]))
}

zipsByProduct(dpID='DP4.00001.001',site="all",
              savepath=dataPath,check.size = F)

for(s in seq_along(NEON_siteNames)[37:47]){
  print(s)
  savePath <- paste0(dataPath,'NEON_SingleAirTemperature/',NEON_siteNames[s])
  zipsByProduct(dpID='DP1.00002.001',site=NEON_siteNames[s],
                savepath=savePath,check.size = F)
  stackByTable(paste0(savePath,'/filesToStack00002'))
}

allPrecipData <- matrix(nrow=0,ncol=5)
colnames(allPrecipData) <- c('siteID','startDateTime','precipBulk','precipExpUncert','precipQF')
for(s in seq_along(NEON_siteNames)[1:47]){
  print(NEON_siteNames[s])
  if(file.exists(paste0(dataPath,"/NEON_PrecipitationData/",NEON_siteNames[s],'/filesToStack00006/stackedFiles/PRIPRE_30min.csv'))){
    subDat <- read.csv(paste0(dataPath,"/NEON_SingleAirTemperature/",NEON_siteNames[s],'/filesToStack00002/stackedFiles/SAAT_30min.csv'))
    subDat <- subDat %>% mutate(precipBulk=priPrecipBulk,precipExpUncert=priPrecipExpUncert,precipQF=priPrecipFinalQF)
  }else{
    subDat <- read.csv(paste0(dataPath,"/NEON_PrecipitationData/",NEON_siteNames[s],'/filesToStack00006/stackedFiles/SECPRE_30min.csv'))
    subDat <- subDat %>% mutate(precipBulk=secPrecipBulk,precipExpUncert=secPrecipExpUncert,precipQF=secPrecipSciRvwQF)
  }
  allPrecipData <- rbind(allPrecipData,subDat[,c('siteID','startDateTime','precipBulk','precipExpUncert','precipQF')])
  
}
write.csv(file=paste0(dataPath,'allPrecipData.csv'),allPrecipData,row.names = FALSE,quote=FALSE)
