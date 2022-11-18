source('sharedVariables.R')
combineNEONdata_PhenoObs <- function(){
  allPhenoObsData <- matrix(nrow=0,ncol=32)
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
  write.csv(file=paste0(dataPath,'NEON_PhenoObservationALLData.csv'),allPhenoObsData,row.names = FALSE,quote=FALSE)
}