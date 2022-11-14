source('sharedVariables.R')
dirs <- list.dirs(path="Data/NEON_struct-plant")
dirs <- dirs[2:length(dirs)]#Remove the NEON_struct_plant parent directory
#dirs <- dirs[grepl('001.2021-', dirs)]
#dirs <- dirs[grepl('BART', dirs)]

taxaSpeciesMatch <- as.data.frame(read.csv('Data/treeAllometricEquations_taxaSpeciesMatch.csv'))
taxaSpeciesMatch$Taxa <- substr(taxaSpeciesMatch$Taxa,1,nchar(taxaSpeciesMatch$Taxa)-1) #Remove white space at the end
taxaSpeciesMatch$scientificName_short <- substr(taxaSpeciesMatch$scientificName_short,1,
                                                nchar(taxaSpeciesMatch$scientificName_short)-1)
taxaSpeciesMatch$Genus <- sapply(strsplit(as.character(taxaSpeciesMatch$scientificName_short),"[[:space:]]"),`[`,1) 

allEqs <- read.csv('Data/treeAllometricEquations.csv') #Table 5 in Chojnacky et al. (2014)
allEqs$Taxa <- substr(allEqs$Taxa,1,nchar(allEqs$Taxa)-1)
# allEqs$family <- sapply(strsplit(as.character(allEqs$Taxa), " "), `[`, 1)
# allEqs$family <- gsub("[[:space:]]","", allEqs$family)
# allEqs$family <- gsub(",","", allEqs$family)
allEqs$Diameter <- gsub("[[:space:]]","", allEqs$Diameter)
allEqs$beta0 <- gsub("[[:space:]]","", allEqs$beta0)
allEqs$beta0 <- gsub("−","-", allEqs$beta0)
allEqs$beta1 <- gsub("[[:space:]]","", allEqs$beta1)
allEqs$beta1 <- gsub("−","-", allEqs$beta1)
#allEqs$family[allEqs$family=="Picea"] <- "Pinaceae"
allEqs <- allEqs[allEqs$Diameter=="dbh",]
taxaSpeciesMatch <- left_join(taxaSpeciesMatch,allEqs,by="Taxa")

allScientificNames <- character()

for(d in (seq_along(dirs))){
  if(d%%100==0){
    print(d)
  }
  if(length(dir(path=dirs[d],pattern="apparentindividual"))>0){
    allScientificNames <- c(allScientificNames,read.csv(paste0(dirs[d],"/",dir(path=dirs[d],pattern="mappingandtagging")))[,'scientificName'])
  }
}

uniqueSN <- unique(allScientificNames)
uniqueSN <- data.frame("sequence"=seq_along(uniqueSN),"name"=uniqueSN)

speciesTaxa <- gbif_species_name_match(df = uniqueSN, name = "name")

mycorrhizaeBiomass <- matrix(nrow=length(dirs),ncol=(length(mycorrhizeTypes)+2))
mycorrhizaeBiomass <- as.data.frame(mycorrhizaeBiomass)
colnames(mycorrhizaeBiomass) <- c("siteID","date",mycorrhizeTypes)

for(d in (seq_along(dirs))[1:length(dirs)]){
  if(d%%10==0){
    print(d)
  }
  mycorrhizaeBiomass$siteID[d] <- strsplit(dirs[d],"[.]")[[1]][3]
  mycorrhizaeBiomass$date[d] <- strsplit(dirs[d],"[.]")[[1]][7]
  #for(d in (1:3)[-1]){
  if(length(dir(path=dirs[d],pattern="apparentindividual"))>0){
    diameterFileDat <- read.csv(paste0(dirs[d],"/",dir(path=dirs[d],pattern="apparentindividual")))[,c('date','siteID','individualID','stemDiameter','basalStemDiameter')]
    speciesFileDat <- read.csv(paste0(dirs[d],"/",dir(path=dirs[d],pattern="mappingandtagging")))[,c('individualID','scientificName')]
    dat <- merge(diameterFileDat,speciesFileDat,by="individualID")
    if(nrow(dat)>0){
    dat$Genus <- sapply(strsplit(as.character(dat$scientificName)," "),`[`,1)
    dat <- left_join(dat,fungalRootDat,by="Genus")
    dat <- left_join(dat,speciesTaxa,by="scientificName")
    
    dat$Genus <- sapply(strsplit(as.character(dat$scientificName)," "),`[`,1)
    
    dat2 <- dat[which(sapply(strsplit(as.character(dat$scientificName)," "),`[`,2)%in%c("sp.","spp.")),]
    uniqueSN <- unique(dat2$Genus)
    for(s in seq_along(uniqueSN)){
      dat[dat$Genus==uniqueSN[s]&is.na(dat$family),'family'] <- na.omit(dat[dat$Genus==uniqueSN[s],'family'])[1]
    }
    
    dat$species <- sapply(strsplit(as.character(dat$scientificName), " "), `[`, 2)
    dat$scientificName_short <- paste(dat$Genus,dat$species)
    
    dat <- left_join(dat,taxaSpeciesMatch[,c("Taxa",'scientificName_short')],by="scientificName_short")
    #Manually assigning taxa for unknown species, but known genus when all genus fall within the same taxa classification
    dat$Taxa[dat$scientificName=="Prunus sp."] <- "Cornaceae/Ericaceae/ Lauraceae/Platanaceae/ Rosaceae/Ulmaceae"
    dat$Taxa[dat$scientificName=="Ulmus sp."] <- "Cornaceae/Ericaceae/ Lauraceae/Platanaceae/ Rosaceae/Ulmaceae"
    dat$Taxa[dat$scientificName=="Carya sp."] <- "Fabaceae/Juglandaceae, Carya"
    dat$Taxa[dat$scientificName=="Pinus sp."] <- "Pinaceae"
    dat <- left_join(dat,allEqs,by="Taxa")
    
    # test <- dat[is.na(dat$beta0),]
    # test2 <- test[!is.na(test$Taxa),]
    dat$stemDiameter[is.na(dat$stemDiameter)] <- dat$basalStemDiameter[is.na(dat$stemDiameter)]
    dat$biomass <- exp(as.numeric(dat$beta0) + as.numeric(dat$beta1)*log(dat$stemDiameter))
    
    iSeq <- which(is.na(dat$biomass))
    for(i in iSeq){
      subDat <- dat[i,]
      potentialTaxas <- taxaSpeciesMatch %>% filter(Genus==subDat$Genus)
      if(nrow(potentialTaxas)>0){
        dat$biomass[i] <- mean(exp(as.numeric(potentialTaxas$beta0) + as.numeric(potentialTaxas$beta1)*log(as.numeric(subDat$stemDiameter))),na.rm=TRUE)
      }
    }
    #test <- dat[is.na(dat$biomass),]
    for(m in seq_along(mycorrhizeTypes)){
      subSubDat <- dat[dat$Mycorrhizal.type==mycorrhizeTypes[m],]
      mycorrhizaeBiomass[d,(m+2)] <- sum(subSubDat$biomass,na.rm=TRUE)/sum(dat$biomass,na.rm=TRUE)
    }
    }
  }
}

write.csv(file="NEON_site_treeBiomass_MTclassifications.csv",mycorrhizaeBiomass,row.names=FALSE,quote = FALSE)

mycorrhizaeBiomass$date <- as.Date(paste(mycorrhizaeBiomass$date,"-01",sep=""))
pdf("NEON_site_biomass_MT.pdf",height=5,width=8)
par(mfrow=c(1,2))
for(siteID in NEON_siteNames){
  siteID <- "BART"
  siteDat <- mycorrhizaeBiomass[mycorrhizaeBiomass$siteID==siteID,]
  row.names(siteDat) <- siteDat$date
  
  #plot(test$date,test$AM,type="l")
  
  
  barplot(t(siteDat[,3:ncol(siteDat)]),col=coul,ylim=c(0,1),las=3,cex.names = 0.5,main=siteID,xlab="Measurement Month",ylab="Ratio by B")
  
  plot.new()
  legend("topleft",legend=mycorrhizeTypes[-c(9,10,13,14)],col=coul[-c(9,10,13,14)],lty=1,lwd=10,cex=0.8,bty="n")
}
dev.off()

