args = commandArgs(trailingOnly=TRUE)
print(args)
my_task_id=as.numeric(args[1])
num_tasks=as.numeric(args[2])
fnames <- 1:47
my_fnames=fnames[seq((my_task_id+1),47,num_tasks)]
# source('sharedVariables.R')
# source('NEON_Data_DownloadAndProcess.R')
# source('computeWeeklyMetData.R')

for(s in my_fnames){
  source('sharedVariables.R')
  source('NEON_Data_DownloadAndProcess.R')
  source('computeWeeklyMetData.R')
  p=3 #Colored leaves
  siteID=NEON_siteNames[s]
  funName="mean"
  dataName="NEON_SingleAirTemperature"

  if(!file.exists(paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",siteID,".csv"))){
    print(NEON_siteNames[s])
    computeTotalMetDataFiles(p=p,siteID=NEON_siteNames[s],dataName=dataName,
                              dataPath=dataPath,funName=funName)
  }

  funName="sum"
  dataName="NEON_PrecipitationData"

  if(!file.exists(paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",siteID,".csv"))){
    print(NEON_siteNames[s])
    computeTotalMetDataFiles(p=p,siteID=NEON_siteNames[s],dataName=dataName,
                             dataPath=dataPath,funName=funName)
  }
  
  funName="mean"
  dataName="NEON_SoilTemp"
  
  if(!file.exists(paste0(dataPath,dataName,"_computedTotalMetData_",gsub(" ","",NEON_phenophase_names[p]),"_",funName,"_",siteID,".csv"))){
    print(NEON_siteNames[s])
    computeTotalMetDataFiles(p=p,siteID=NEON_siteNames[s],dataName=dataName,
                             dataPath=dataPath,funName=funName)
  }
}
