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

dataName="NEON_SoilTemp"
NEON_ID='DP1.00041.001'
if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}

downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack=FALSE) #missing 30 

selectColumns <- c('horizontalPosition','verticalPosition','soilTempMean','soilTempMinimum','soilTempMaximum','finalQF')
inFileName <- "ST_30_minute.csv"

combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,selectColumns=selectColumns,
                inFileName=inFileName,dataPath=dataPath)
