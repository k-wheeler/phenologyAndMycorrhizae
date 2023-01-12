print("entered script")

source('sharedVariables.R')
source('NEON_Data_DownloadAndProcess.R')

print("loaded packages")
dataName="NEON_PAR"
NEON_ID='DP1.00024.001'
if(!file.exists(paste0(dataPath,"/",dataName))){
  dir.create(paste0(dataPath,"/",dataName))
}
print("Created directories")
# selectColumns <- c('precipBulk','precipExpUncert','precipQF')
# inFileName <- c("PRIPRE_30min.csv","SECPRE_30min.csv")
# varName <- "precipBulk"

downloadNEONdata(dataName=dataName,NEON_ID=NEON_ID)
print("finished downloading data")