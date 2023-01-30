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
dataName="NEON_TemperatureData"
NEON_ID='DP1.00003.001'
selectColumns <- c('tempTripleMean','finalQF')
inFileName <- "TAAT_30min.csv"
varName <- 'tempTripleMean'

if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}
downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID)
#}



