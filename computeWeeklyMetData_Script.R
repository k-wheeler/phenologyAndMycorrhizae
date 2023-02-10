args = commandArgs(trailingOnly=TRUE)
print(args)
my_task_id=as.numeric(args[1])
num_tasks=as.numeric(args[2])
fnames <- 1:47
my_fnames=fnames[seq((my_task_id+1),47,num_tasks)]

for(s in my_fnames){
  source('sharedVariables.R')
  source('NEON_Data_DownloadAndProcess.R')
  source('computeWeeklyMetData.R')
  funName="mean"
  dataName="NEON_SingleAirTemperature"
  computeWeeklyMetDataFiles(p=2,siteID=NEON_siteNames[s],dataName=dataName,
                            dataPath=dataPath,funName=funName,nWeeks=8) 
  
  funName="sum"
  dataName="NEON_PrecipitationData"
  computeWeeklyMetDataFiles(p=2,siteID=NEON_siteNames[s],dataName=dataName,
                            dataPath=dataPath,funName=funName,nWeeks=8) 
}