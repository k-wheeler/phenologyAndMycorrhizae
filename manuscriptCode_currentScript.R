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

selectColumns <- c('horizontalPosition','verticalPosition','soilTempMean','soilTempMinimum','soilTempMaximum','finalQF')
inFileName <- "ST_30_minute.csv"
varName <- 'soilTempMean'

combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,selectColumns=selectColumns,
                inFileName=inFileName,dataPath=dataPath)

funType=mean
funName="mean"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)

funType=min
funName="min"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)

funType=max
funName="max"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)


