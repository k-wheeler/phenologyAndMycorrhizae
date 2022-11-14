source('sharedVariables.R')

s=2
for(s in seq_along(NEON_siteNames)[37:47]){
  print(s)
  zipsByProduct(dpID='DP1.00003.001',site=NEON_siteNames[s],
                savepath=dataPath,check.size = F)
}

allTempData <- matrix(nrow=0,ncol=4)
for(s in seq_along(NEON_siteNames)[2:47]){
  print(NEON_siteNames[s])
  #dir.create(paste0(dataPath,'NEON_TemperatureData/',NEON_siteNames[s]))
  #stackByTable(paste0(dataPath,"/NEON_TemperatureData/",NEON_siteNames[s]),folder=T)
  subDat <- read.csv(paste0(dataPath,"/NEON_TemperatureData/",NEON_siteNames[s],'/stackedFiles/TAAT_30min.csv'))
  allTempData <- rbind(allTempData,subDat[,c('siteID','startDateTime','tempTripleMean','finalQF')])
}
write.csv(file=paste0(dataPath,'allTemperatureData.csv'),allTempData,row.names = FALSE,quote=FALSE)

subDat <- allTempData %>% filter(siteID=="HARV")

