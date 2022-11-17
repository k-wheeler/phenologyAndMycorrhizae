source('sharedVariables.R')

s=2
for(s in seq_along(NEON_siteNames)){
  print(NEON_siteNames[s])
  dir.create(paste0(dataPath,'NEON_PhenologyObservations/',NEON_siteNames[s]))
}

for(s in seq_along(NEON_siteNames)[2:47]){
  print(s)
  savePath <- paste0(dataPath,'NEON_PhenologyObservations/',NEON_siteNames[s])
  zipsByProduct(dpID='DP1.10055.001',site=NEON_siteNames[s],
                savepath=savePath,check.size = F)
  stackByTable(paste0(savePath,'/filesToStack10055'))
}

allPhenoObsData <- matrix(nrow=0,ncol=32)
#colnames(allPrecipData) <- c('siteID','startDateTime','precipBulk','precipExpUncert','precipQF')
for(s in seq_along(NEON_siteNames)[1:47]){
  print(NEON_siteNames[s])

  pheDat <- read.csv(paste0(dataPath,"/NEON_PhenologyObservations/",NEON_siteNames[s],'/filesToStack10055/stackedFiles/phe_statusintensity.csv'))
  pheDat <- pheDat %>% mutate(dataQF_phenoStatus=dataQF)
  pheDat <- pheDat[,c('siteID', 'plotID', 'date', 'dayOfYear', 'individualID',
                      'phenophaseName','phenophaseStatus','phenophaseIntensityDefinition','phenophaseIntensity','dataQF_phenoStatus')]
  
  
  pheChar <- read.csv(paste0(dataPath,"/NEON_PhenologyObservations/",NEON_siteNames[s],'/filesToStack10055/stackedFiles/phe_perindividualperyear.csv'))
  pheChar <- pheChar %>% mutate(dataQF_characteristics=dataQF)
  pheChar <- pheChar[,c('individualID','patchOrIndividual',
                        'canopyPosition','plantStatus','stemDiameter','measurementHeight',
                        'maxCanopyDiameter','ninetyCanopyDiameter','patchSize','percentCover','height',
                        'diseaseType','dataQF_characteristics')]
  pheDat <- left_join(pheDat,pheChar,by="individualID")
  
  indChar <- read.csv(paste0(dataPath,"/NEON_PhenologyObservations/",NEON_siteNames[s],'/filesToStack10055/stackedFiles/phe_perindividual.csv'))
  indChar <- indChar[,c('individualID','decimalLatitude','decimalLongitude','elevation',
                        'elevationUncertainty','subtypeSpecification','scientificName','nativeStatusCode',
                        'growthForm')]
  pheDat <- left_join(pheDat,indChar,by="individualID")
  pheDat$Genus <- sapply(strsplit(as.character(pheDat$scientificName)," "),`[`,1)
  pheDat <- left_join(pheDat,fungalRootDat,by="Genus")
  allPhenoObsData <- rbind(allPhenoObsData,pheDat)
  
}
write.csv(file=paste0(dataPath,'allPhenoObservationData.csv'),allPhenoObsData,row.names = FALSE,quote=FALSE)

#subDat <- allTempData %>% filter(siteID=="HARV")
