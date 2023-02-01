#Plot Data
source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

dataNames <- c("NEON_SingleAirTemperature","NEON_PrecipitationData","NEON_PAR",
               "NEON_relativeHumidity","NEON_Windspeed","NEON_SoilTemp")
varNames <- c("tempSingleMean","precipBulk","PARMean","RHMean","windSpeedMean","soilTempMean")
pdf('NEON_metData.pdf',height=5,width=12)
for(i in seq_along(dataNames)){
  dataName <- dataNames[i]
  varName <- varNames[i]
  if(dataName=="NEON_SingleAirTemperature"){
    meanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_mean_gapFilled.csv"),header=TRUE)
  }else if(dataName=="NEON_PrecipitationData"){
    meanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_gapFilled.csv"),header=TRUE)
  }else{
    meanDat <- read.csv(paste0(dataPath,dataName,"Dailydata_mean.csv"),header=TRUE)
  }
  # minDat <- read.csv(paste0(dataPath,dataName,"Dailydata_min_gapFilled.csv"), header=TRUE)
  # maxDat <- read.csv(paste0(dataPath,dataName,"Dailydata_max_gapFilled.csv"), header=TRUE)
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    siteMeanDat <- meanDat %>% filter(siteID==siteName)
    colnames(siteMeanDat)[4] <- "value"
    ggplot(siteMeanDat,aes(x=as.Date(date),y=value,col=verticalPosition)) +
      geom_point()+
      theme_classic()+
      ggtitle(siteName)+
      xlab("Date")+
      ylab(dataName)
    
  }
}
dev.off()
