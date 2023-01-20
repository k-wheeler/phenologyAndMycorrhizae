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

dataName="NEON_Roots"
NEON_ID='DP1.10067.001'

if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}

inFileName <- 'bbc_dilution'
selectColumns <- c('plotID','sampleVolume',
                   'dryMass','somDryMass')

dat1 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
                        selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)

inFileName <- 'bbc_percore'
selectColumns <- c('plotID','decimalLatitude','decimalLongitude',
                   'litterDepth','rootSampleDepth')

dat2 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
                        selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)

allDat <- full_join(dat1,dat2,by=c('siteID','collectDate','plotID'))

inFileName <- 'bbc_rootChemistry'
selectColumns <- c('plotID','plotType',
                   'd15N','d13C','nitrogenPercent','carbonPercent',
                   'CNratio','cnIsotopeQF','cnPercentQF')

dat3 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
                        selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)

allDat <- full_join(allDat,dat3,by=c('siteID','collectDate','plotID'))

inFileName <- "bbc_rootmass"
selectColumns <- c('plotID','dryMass','mycorrhizaeVisible')

dat4 <- combineNEONdata(dataName=dataName,NEON_ID=NEON_ID,
                        selectColumns=selectColumns, inFileName=inFileName,dataPath=dataPath,saveFile = FALSE)

allDat <- full_join(allDat,dat4,by=c('siteID','collectDate','plotID'))
