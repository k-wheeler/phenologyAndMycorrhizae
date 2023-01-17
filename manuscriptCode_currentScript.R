print("entered script")

source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

print("loaded packages")
# dataName="NEON_SoilTemp"
# NEON_ID='DP1.00041.001'
# dataName="NEON_Roots"
# NEON_ID='DP1.10067.001'
# dataName="NEON_relativeHumidity"
# NEON_ID='DP1.00098.001'

# dataName="NEON_plantFoliarTraits"
# NEON_ID="DP1.10026.001"
# 
# 
dataName="NEON_litterfall" #No: 2, 7,8
NEON_ID="DP1.10033.001"
# 
# dataName="NEON_canopyWaterIndices"
# NEON_ID="DP3.30019.001"

if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}

downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID,andStack=FALSE,
                 includedSeq=seq(9,47))
print("downloaded data")

# IDnum <- strsplit(NEON_ID,"[.]")[[1]][2]
# for(s in seq_along(NEON_siteNames)[4:47]){
#   print(s)
#   savePath <- paste0(dataPath,dataName,'/',NEON_siteNames[s])
#   stackByTable(paste0(savePath,'/filesToStack',IDnum))
# }