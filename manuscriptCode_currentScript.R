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

dataName="NEON_relativeHumidity"
NEON_ID='DP1.00098.001'
# dataName="NEON_Windspeed"
# NEON_ID='DP1.00001.001'

selectColumns <- c('verticalPosition','RHMean','RHMinimum','RHMaximum','RHVariance','dewTempMean',
                   'RHFinalQF','dewTempFinalQF')
inFileName <- "RH_30min.csv"
varName <- "RHMean"

# combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,selectColumns=selectColumns,
#                 inFileName=inFileName,dataPath=dataPath)

funType=mean
funName="mean"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)

funType=min
funName="min"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)

funType=max
funName="max"
calculateDailyWeather(dataName=dataName,dataPath=dataPath,varName=varName,funType=funType,funName=funName)

