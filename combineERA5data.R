source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

combineERA5data <- function(siteID){
  lat <- siteData$field_latitude[siteData$field_site_id==siteID]
  long <- siteData$field_longitude[siteData$field_site_id==siteID]
  siteDat <- matrix(nrow=0,ncol=7)
  for(yr in 2014:2022){
    start_date <- as.Date(paste0(yr,"-01-01"))
    end_date <- as.Date(paste0(yr,"-12-31"))
    
    fileName <- paste0(dataPath,'ERA5/',siteID,"_",start_date,"_",end_date,"_era5Members.nc")
    print(fileName)
    if(file.exists(fileName)){
      nc <- nc_open(fileName)
      vars <- names(nc$var)
      time <- as.integer(ncdf4::ncvar_get(nc, "time"))
      time <- as.POSIXct(time*3600, origin = "1900-01-01",tz = "GMT")
      time <- as.Date(time)
      for(v in vars){
        dat <- ncvar_get(nc,varid=v)
        if(yr==2022){ #For some reason, 2022 has an extra NAs. 
          dat <- dat[,1,]
        }
        dat <- colMeans(dat)
        datTime <- data.frame(var=dat,date=time) %>% group_by(date) %>% 
          summarise(mean=mean(var),min=min(var),max=max(var)) %>%
          pivot_longer(cols=2:4,names_to="funName",values_to="value") %>%
          mutate(siteID=siteID,lat=lat,long=long,var=v)
        siteDat <- rbind(siteDat,datTime)
      }
      nc_close(nc)
    }
  }
  return(siteDat)
}

named.list <- lapply(NEON_siteNames,combineERA5data)

allDat.matrix <-rbindlist(named.list)

write.csv(file=paste0(dataPath,"ERA5_metData.csv"),allDat.matrix,row.names = FALSE,quote = FALSE)