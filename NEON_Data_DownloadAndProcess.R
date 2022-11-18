downloadNEONdata <- function(dataName,NEON_ID){
  #Create Site-Specific Folders for Downloading 
  for(s in seq_along(NEON_siteNames)){
    print(NEON_siteNames[s])
    dir.create(paste0(dataPath,dataName,'/',NEON_siteNames[s]))
  }
  #Download and Stack the Data
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  for(s in seq_along(NEON_siteNames)){
    print(s)
    savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
    zipsByProduct(dpID=NEON_ID,site=NEON_siteNames[s],
                  savepath=savePath,check.size = F)
    
    stackByTable(paste0(savePath,'/filesToStack',IDnum))
  }
}

combineNEONdata <- function(dataName,NEON_ID,selectColumns,inFileName,dataPath){
  allData <- matrix(nrow=0,ncol=(length(selectColumns)+2))
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  if(length(inFileName)==1){
  for(s in seq_along(NEON_siteNames)){
    print(NEON_siteNames[s])
    subDat <- read.csv(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName))
    
    allData <- rbind(allData,subDat[,c('siteID','startDateTime',selectColumns)])
  }
  }else if(length(inFileName)==2){
    for(s in seq_along(NEON_siteNames)){
      print(NEON_siteNames[s])
      potentialFile1 <- paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName[1])
      if(file.exists(potentialFile1)){
        subDat <- read.csv(potentialFile1)
        subDat <- subDat %>% mutate(precipBulk=priPrecipBulk,precipExpUncert=priPrecipExpUncert,precipQF=priPrecipFinalQF)
      }else{
        subDat <- read.csv(paste0(dataPath,dataName,"/",NEON_siteNames[s],'/filesToStack',IDnum,'/stackedFiles/',inFileName[2]))
        subDat <- subDat %>% mutate(precipBulk=secPrecipBulk,precipExpUncert=secPrecipExpUncert,precipQF=secPrecipSciRvwQF)
      }
      allData <- rbind(allData,subDat[,c('siteID','startDateTime','precipBulk','precipExpUncert','precipQF')])
    }
  }
  outFileName <- paste0(dataName,"ALLdata.csv")
  write.csv(file=paste0(dataPath,outFileName),allData,row.names = FALSE,quote=FALSE)
}

calculateDailyWeather <- function(dataName,dataPath,varName,funType){
  inFileName <- paste0(dataName,"ALLdata.csv")
  outFileName <- paste0(dataName,"Dailydata.csv")
  allData <- read.csv(file=paste0(dataPath,inFileName))
  allDaily <- matrix(nrow=0,ncol=3)
  if("precipQF" %in% colnames(allData)){
    names(allData)[names(allData)=="precipQF"] <- 'finalQF'
  }
  for(s in seq_along(NEON_siteNames)){
    siteName <- NEON_siteNames[s]
    print(siteName)
    allData$oldValue <- allData[,varName]
    subDat <- allData %>% filter(siteID==siteName) %>% 
      filter(finalQF==0 | is.na(finalQF)) %>%
      mutate(date = lubridate::floor_date(as.POSIXct(startDateTime))) %>%
      group_by(date) %>% mutate(value=funType(oldValue,na.rm=TRUE))
    subDat <- data.frame(subDat)
    subDat <-subDat[,which(names(subDat) %in% c('siteID','date','value'))] %>% distinct()
    allDaily <- rbind(allDaily,subDat)
  }
  names(allDaily)[names(allDaily)=="value"] <- varName
  write.csv(file=paste0(dataPath,outFileName),allDaily,row.names = FALSE,quote=FALSE)
  
}
