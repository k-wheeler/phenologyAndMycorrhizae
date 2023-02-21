source('sharedVariables.R')
combineNEONdata_PhenoObs <- function(saveType="",p=""){
  allPhenoObsDat <- matrix(nrow=0,ncol=32)
  for(s in seq_along(NEON_siteNames)[1:47]){
    print(NEON_siteNames[s])
    
    pheDat <- read.csv(paste0(dataPath,"/NEON_PhenologyObservations/",NEON_siteNames[s],'/filesToStack10055/stackedFiles/phe_statusintensity.csv'))
    pheDat <- pheDat %>% mutate(dataQF_phenoStatus=dataQF)
    pheDat <- pheDat[,c('siteID', 'plotID', 'date', 'dayOfYear', 'individualID',
                        'phenophaseName','phenophaseStatus','phenophaseIntensityDefinition','phenophaseIntensity','dataQF_phenoStatus')]
    if(p!=""){
      pheDat <- pheDat %>% filter(phenophaseName==NEON_phenophase_names[p])
    }
    if(nrow(pheDat)>0){
      
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
      allPhenoObsDat <- rbind(allPhenoObsDat,pheDat)
      if(saveType=="bySite"){
        write.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',NEON_siteNames[s],'.csv'),pheDat,row.names = FALSE,quote=FALSE)
      }
    }
    
  }
  allPhenoObsDat$year <- lubridate::year(allPhenoObsDat$date)
  allPhenoObsDat$plantStatus <- gsub(",","",allPhenoObsDat$plantStatus)
  allPhenoObsDat$diseaseType <- gsub(",","",allPhenoObsDat$diseaseType)
  allPhenoObsDat$dayOfYear <- lubridate::yday(allPhenoObsDat$date)
  if(saveType=="firstOfPhenophase"){
    for(p in seq_along(NEON_phenophase_names)){
      print(NEON_phenophase_names[p])
      subDat <- allPhenoObsDat %>% filter(phenophaseName==NEON_phenophase_names[p],phenophaseStatus=="yes",phenophaseIntensity!="") %>% group_by(individualID,phenophaseIntensity,year) %>% slice(1:1)
      write.csv(subDat,file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',gsub(" ","",NEON_phenophase_names[p]),'.csv'),row.names=FALSE,quote=FALSE)
    }
  }else if(saveType=="allOfPhenophase"){
    p <- 5 #Just done for the Leaves phenophase
    print(NEON_phenophase_names[p])
    subDat <- allPhenoObsDat %>% filter(phenophaseName==NEON_phenophase_names[p],phenophaseStatus=="yes",phenophaseIntensity!="")
    write.csv(subDat,file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllIntensities_',gsub(" ","",NEON_phenophase_names[p]),'.csv'),row.names=FALSE,quote=FALSE)
  }else if(saveType=="allStatusPhenophase"){
    write.csv(allPhenoObsDat,file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllStatus_',gsub(" ","",NEON_phenophase_names[p]),'.csv'),row.names=FALSE,quote=FALSE)
  }
  
}

determinePhenophaseTimingForLeaves <- function(dataPath){
  p <- 5
  phenoDat <- read.csv(file=paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_AllIntensities_',gsub(" ","",NEON_phenophase_names[p]),'.csv'))
  lowIntDat <- phenoDat %>% filter(phenophaseIntensity == "5-24%") %>% dplyr::select(-c(phenophaseIntensity,phenophaseStatus,phenophaseIntensityDefinition))
  greenUpDat <- lowIntDat %>% group_by(individualID,year) %>% slice(1:1) %>% rename(greenupDate=date,greenupDOY=dayOfYear)
  
  highIntDat <- phenoDat %>% filter(phenophaseIntensity == "75-94%") %>% 
    dplyr::select(-c(phenophaseIntensity,phenophaseStatus,phenophaseIntensityDefinition)) %>% 
    group_by(individualID,year) %>% slice(1:1)
  
  greenDownDat <- full_join(lowIntDat,highIntDat,by=c('siteID','plotID','individualID','decimalLatitude',
                                                      'decimalLongitude','elevation','scientificName','growthForm','Genus','Mycorrhizal.type','year')) %>% 
    filter(date.x > date.y) %>% 
    group_by(individualID,year) %>% 
    slice(1:1) %>%
    ungroup() %>%
    dplyr::select(-date.y) %>%
    rename(greendownDate=date.x,greendownDOY=dayOfYear.x) %>%
    dplyr::select(individualID,year,greendownDate,greendownDOY)
  
  leavesPhenophases <- full_join(greenUpDat,greenDownDat,by=c('individualID','year'))
  return(leavesPhenophases)
}

s=14
pheDat <- read.csv(paste0(dataPath,'NEON_PhenologyObservations/NEON_PhenoObservationData_',NEON_siteNames[s],'.csv'))
subDat <- pheDat %>% filter(individualID=="NEON.PLA.D01.HARV.06019",phenophaseName=="Colored leaves")
subDat <- pheDat %>% filter(phenophaseName=="Colored leaves")
subDat <- pheDat %>% filter(phenophaseIntensity=="5-24%")


