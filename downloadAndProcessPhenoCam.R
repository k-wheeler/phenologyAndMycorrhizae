#Download and Process PhenoCam Data
source('sharedVariables.R')
source("downloadPhenoCam.R")
source("calculatePhenoCamUncertainty.R")

phenoCamURLs <- read.csv(paste0(dataPath,'phenocamURLS.csv'))
allData <- data.frame(matrix(nrow = 0, ncol = 6))

for(i in 3:nrow(phenoCamURLs)){
  siteName <- phenoCamURLs$phenocam_id[i]
  site_roi <- phenoCamURLs$ROI[i]
  message(siteName)
  print(i)
  ##URL for daily summary statistics
  URL_gcc90 <- phenoCamURLs$URL_gcc90[i]
  ##URL for individual image metrics
  URL_individual <- paste('https://phenocam.nau.edu/data/archive/',siteName,"/ROI/",siteName,"_",site_roi,"_roistats.csv",sep="") 
  
  phenoData <- download.phenocam(URL = URL_gcc90)
  dates <- unique(phenoData$date)
  phenoData_individual <- download.phenocam(URL=URL_individual,skipNum = 17)
  ##Calculates standard deviations on daily gcc90 values
  gcc_sd <- calculate.phenocam.uncertainty(dat=phenoData_individual,dates=dates)    

  subPhenoData <- phenoData %>% 
    mutate(site_id = stringr::str_sub(siteName, 10, 13), 
           time = date) %>% 
    select(time, site_id, gcc_90) %>% 
    pivot_longer(-c("time", "site_id"), names_to = "variable", values_to = "observed") %>% 
    mutate(sd = ifelse(variable == "gcc_90", gcc_sd))
  
  subPhenoData$ROI <- site_roi
  
  allData <- rbind(allData,subPhenoData)
}
allPhenoCamData <- allData
save(allPhenoCamData,file=paste0(dataPath,"all_PhenoCam_gcc_data.RData"))
