#Identify top-of-canopy temperature sensors
source('sharedVariables.R')
dataName="NEON_SingleAirTemperature"
funName="mean"
metDat <- read.csv(paste0(dataPath,dataName,'Dailydata_',funName,'_ERAgapFilled.csv'))

maxVerticalHeights <- metDat %>% dplyr::select(siteID,verticalPosition) %>% unique() %>%
  group_by(siteID) %>% summarise(maxHeight=max(verticalPosition))

write.csv(maxVerticalHeights,file="NEON_site_maxHeights.csv",row.names=FALSE,quote=FALSE)