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

# dataName="NEON_RH"
# NEON_ID='DP1.00098.001'
dataName="NEON_Windspeed"
NEON_ID='DP1.00001.001'

if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}
# selectColumns <- c('verticalPosition','PARMean','PARMinimum','PARMaximum','PARVariance','PARFinalQF')
# inFileName <- "PARPAR_30min.csv"
# varName <- "PARMean"

downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack = FALSE)
