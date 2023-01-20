args = commandArgs(trailingOnly=TRUE)
print(args)
my_task_id=as.numeric(args[1])
num_tasks=as.numeric(args[2])
fnames <- 10:47
my_fnames=fnames[seq((my_task_id+1),47,num_tasks)]

for(s in my_fnames){
  
  source('sharedVariables.R')
  source('NEON_Data_DownloadAndProcess.R')
  
  # print("loaded packages")
  # dataName="NEON_SoilTemp"
  # NEON_ID='DP1.00041.001'
  
  dataName="NEON_PAR"
  NEON_ID='DP1.00024.001'
  
  IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  
  print(s)
  savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  fls <- dir(path=paste0(savePath,'/filesToStack',IDnum),pattern="NEON")
  if(file.exists(paste0(savePath,'/filesToStack',IDnum))
     &(length(fls)>0)){
    stackByTable(paste0(savePath,'/filesToStack',IDnum))
  }
  
  # if(!file.exists(paste0(dataPath,"/",dataName))){
  #   dir.create(paste0(dataPath,"/",dataName))
  # }
  
  # downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack=FALSE,
  #                  includedSeq=seq(46,47))
  # dataName="NEON_Roots"
  # NEON_ID='DP1.10067.001'
  
  # IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  # for(s in seq_along(NEON_siteNames)[1:47]){
  #   print(s)
  #   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  #   if(file.exists(paste0(savePath,'/filesToStack',IDnum))){
  #     stackByTable(paste0(savePath,'/filesToStack',IDnum))
  #   }
  # }
  # 
  # dataName="NEON_relativeHumidity"
  # NEON_ID='DP1.00098.001'
  # 
  # IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  # for(s in seq_along(NEON_siteNames)[1:47]){
  #   print(s)
  #   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  #   if(file.exists(paste0(savePath,'/filesToStack',IDnum))){
  #     stackByTable(paste0(savePath,'/filesToStack',IDnum))
  #   }
  # }
  # 
  # dataName="NEON_plantFoliarTraits"
  # NEON_ID="DP1.10026.001"
  # 
  # IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  # for(s in seq_along(NEON_siteNames)[1:47]){
  #   print(s)
  #   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  #   if(file.exists(paste0(savePath,'/filesToStack',IDnum))){
  #     stackByTable(paste0(savePath,'/filesToStack',IDnum))
  #   }
  # }
  # # 
  # # 
  # dataName="NEON_litterfall" #No: 2, 7,8, 11, 17, 18, 20, 23, 25, 27, 38, 41, 45
  # NEON_ID="DP1.10033.001"
  # 
  # IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
  # for(s in seq_along(NEON_siteNames)[1:47]){
  #   print(s)
  #   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
  #   if(file.exists(paste0(savePath,'/filesToStack',IDnum))){
  #     stackByTable(paste0(savePath,'/filesToStack',IDnum))
  #   }
  # }
  # 
  # 
  # # dataName="NEON_canopyWaterIndices"
  # # NEON_ID="DP3.30019.001"
  
  # if(!file.exists(paste0(dataPath,"/",dataName))){
  #   dir.create(paste0(dataPath,"/",dataName))
  # }
  # 
  # downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack=FALSE,
  #                  includedSeq=seq(46,47))
  # print("downloaded data")
  
}
