print("entered script")

source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

print("loaded packages")
dataName="NEON_SoilTemp"
NEON_ID='DP1.00041.001'
if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}

downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack=FALSE,
                 includedSeq=seq(30,47))
print("downloaded data")

# IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
# for(s in seq_along(NEON_siteNames)[4:47]){
#   print(s)
#   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
#   stackByTable(paste0(savePath,'/filesToStack',IDnum))
# }