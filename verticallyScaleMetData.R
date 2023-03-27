#Looking into vertically scaling weather data
#Look at if the temperature data along the tower is all taken together
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


load("fittedAirTempModels.RData")

for(s in seq_along(NEON_siteNames)){
  site <- NEON_siteNames[s]
  siteDat <- fittedModelSummary %>% filter(siteID==site)
  plot(density(as.numeric(siteDat$r.squared)),main=site)
  siteDat <- siteDat %>% filter(r.squared>0.9,term=="verticalPosition") %>% 
    mutate(date=as.Date(startDateTime)) %>%
    group_by(date)%>%summarise(n=n())
  plot(as.Date(siteDat$startDateTime),siteDat$r.squared,pch=20,main=site)
  plot(siteDat$date,siteDat$n,pch=20,main=site,type="l")
  
}
