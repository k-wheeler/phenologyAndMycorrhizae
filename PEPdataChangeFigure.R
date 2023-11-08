#Investigating PEP Phenology Data
library('data.table')
library('tidyverse')
library(ggplot2)
library(gridExtra)

dataFolder <- "Data/"
PEPstationFileName <- paste0(dataFolder,'allPEPstationDat.csv')
phenophases <- c(11,13,94,95)

##Load and Process in PEP station data ----

if(!file.exists(PEPstationFileName)){
  stationFiles <- paste0("Data/PEP/stationDat/",dir(path="Data/PEP/stationDat/",pattern="station"))
  stationDat <- read.table(stationFiles[1],header=TRUE,sep=";")
  
  for(f in 2:length(stationFiles)){
    print(f)
    stationDat <- rbind(stationDat,read.table(stationFiles[f],header=TRUE,sep=";"))
  }
  
  write.csv(file=PEPstationFileName,stationDat,quote=FALSE,row.names=FALSE)
}else{
  stationDat <- read.csv(PEPstationFileName)
}
stationDat <- stationDat %>% select(-NAME) %>% mutate(PEP_ID = as.numeric(PEP_ID))

##Load in PEP Data ----
PEPfiles <- dir(path=paste0(dataFolder,"PEP"),pattern=".csv")

allPep <- lapply(PEPfiles, function(X){
  newPepDat <- read.table(paste0('Data/PEP/',X),sep=";",header=TRUE)
  spl <- strsplit(strsplit(X,"[.]")[[1]][1],"_")[[1]]
  newPepDat$species <- paste(spl[3:length(spl)],collapse="_")
  return(newPepDat)
})

allPep <- rbindlist(allPep)

allPep <- allPep %>% filter(BBCH %in% phenophases)
allPep <- left_join(allPep,stationDat,by="PEP_ID")

allPep$Genus <- unlist(lapply(allPep$species,function(X){
  return(strsplit((strsplit(X,"[(]")[[1]][1]),"_")[[1]][1])
}))

fungalRootDat <- read.csv(paste0(dataFolder,"fungalRootAssociations.csv"))

allPep <- left_join(allPep,fungalRootDat,by="Genus")

allPep <- allPep %>% filter(Mycorrhizal.type %in% c("AM","EcM"))

bbchNames <- read.table(paste0(dataFolder,'PEP725_BBCH.csv'),sep=";",header=TRUE)

includedStations <- numeric()

jpeg(filename='mycorrhizaeTypeChanges_PEP_30years_Figure.jpeg',width=8,height=8,units="in",res=1000)

par(mfrow=c(4,2))
for(p in seq_along(phenophases)){
  #for(p in 1:2){
  phenoDescription <- bbchNames[bbchNames$bbch==phenophases[p],2]
  if(p==1){
    phenoDescription <- "Leaf unfolding start"
  }
  print(phenoDescription)
  phenoPep <- allPep %>% filter(BBCH == phenophases[p])
  
  groupedDat <- phenoPep %>% group_by(PEP_ID,YEAR,Mycorrhizal.type) %>% summarize(perYearCount=n())
  groupedDat <-  groupedDat %>% group_by(PEP_ID,Mycorrhizal.type) %>% summarize(numYears=n(),avgPerYearCount=mean(perYearCount))
  
  groupedDat <- groupedDat %>% pivot_wider(names_from=Mycorrhizal.type,values_from=c(numYears,avgPerYearCount))
  
  bothLongDat <- groupedDat %>% filter(numYears_AM>29 & numYears_EcM>29)
  
  longSites <- bothLongDat$PEP_ID
  print(length(longSites))
  includedStations <- unique(c(includedStations,longSites))
  
  longDat <- phenoPep %>% filter(PEP_ID %in% longSites)
  
  pepDat_AM <- longDat %>% filter(Mycorrhizal.type=="AM")
  pepDat_EcM <- longDat %>% filter(Mycorrhizal.type=="EcM")

  sumStats <- matrix(nrow=0,ncol=4)
  
  for(i in seq_along(longSites)){
    siteID=longSites[i]
    site_AM <- pepDat_AM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_AM)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"AM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
    
    site_EcM <- pepDat_EcM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_EcM)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"EcM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
  }  
  
  sumStats <- data.frame(sumStats)
  colnames(sumStats) <- c('PEP_ID',"Mycorrhizal.type","change","change_p")
  
  combinedStats <- sumStats %>% select(-change_p) %>% 
    pivot_wider(names_from=Mycorrhizal.type,values_from = change)  
  
  plot(density(as.numeric(sumStats[sumStats[,2]=="EcM",3])),col="blue",xlim=range(as.numeric(sumStats[,3])),lwd=2, main=phenoDescription,xlab="Change (Days/year)")
  lines(density(as.numeric(sumStats[sumStats[,2]=="AM",3])),col="orange",lwd=2)
  legend('topleft',col=c('blue','orange'),lwd=2,c(paste("EcM Mean:",round(mean(as.numeric(sumStats[sumStats[,2]=="EcM",3])),digits=2)),paste("AM Mean:",round(mean(as.numeric(sumStats[sumStats[,2]=="AM",3]),),digits=2))),bty = "n")
  
  plot(density(as.numeric(combinedStats$EcM)-as.numeric(combinedStats$AM)),lwd=2,main=phenoDescription,xlab="Difference in Change (Days/year)")

  abline(v=0,col="gray",lty=2,lwd=2)
  test <- t.test(as.numeric(combinedStats$EcM),as.numeric(combinedStats$AM),paired = TRUE)
  
  legend('topright',col='white',pch=".",
         paste('Difference Mean:',round(mean(as.numeric(combinedStats$EcM)-as.numeric(combinedStats$AM)),digits=4)),bty = "n")
}
dev.off()


###Changes based off of photoperiod sensitivity ----
zohnerDat <- read.csv('zohnerPhotoperiodSensitivity.csv') %>% select(Genus, Photo)

allPep <- left_join(allPep,zohnerDat,by="Genus")
allP <- list()
for(p in seq_along(phenophases)){
  #for(p in 1:2){
  phenoDescription <- bbchNames[bbchNames$bbch==phenophases[p],2]
  if(p==1){
    phenoDescription <- "Leaf unfolding start"
  }
  print(phenoDescription)
  phenoPep <- allPep %>% filter(BBCH == phenophases[p])
  
  groupedDat <- phenoPep %>% group_by(PEP_ID,YEAR,Photo,Mycorrhizal.type) %>% summarize(perYearCount=n())
  groupedDat <-  groupedDat %>% group_by(PEP_ID,Photo,Mycorrhizal.type) %>% summarize(numYears=n(),avgPerYearCount=mean(perYearCount))
  
  groupedDat <- groupedDat %>% pivot_wider(names_from=c(Photo,Mycorrhizal.type),values_from=c(numYears,avgPerYearCount))
  
  bothLongDat <- groupedDat %>% filter(numYears_Low_AM>29 & numYears_Low_EcM>29 & numYears_None_AM>29 & numYears_None_EcM>29)
  
  longSites <- bothLongDat$PEP_ID
  print(length(longSites))
  
  longDat <- phenoPep %>% filter(PEP_ID %in% longSites)
  
  pepDat_low_AM <- longDat %>% filter(Photo=="Low" & Mycorrhizal.type =="AM")
  pepDat_none_AM <- longDat %>% filter(Photo=="None" & Mycorrhizal.type =="AM")
  
  pepDat_low_EcM <- longDat %>% filter(Photo=="Low" & Mycorrhizal.type =="EcM")
  pepDat_none_EcM <- longDat %>% filter(Photo=="None" & Mycorrhizal.type =="EcM")
  
  sumStats <- matrix(nrow=0,ncol=4)
  
  for(i in seq_along(longSites)){
    siteID=longSites[i]
    site_low <- pepDat_low_AM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_low)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"Low_AM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
    
    site_low <- pepDat_low_EcM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_low)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"Low_EcM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
    
    site_none <- pepDat_none_AM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_none)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"None_AM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
    
    site_none <- pepDat_none_EcM %>% filter(PEP_ID==siteID)
    mdl <- lm(DAY~YEAR,data=site_none)
    mdlSum <- summary(mdl)
    sumStats <- rbind(sumStats,c(siteID,"None_EcM",mdl$coefficients[2],mdlSum$coefficients[2,4]))
  }  
  
  sumStats <- data.frame(sumStats)
  colnames(sumStats) <- c('PEP_ID',"Photo_MT","change","change_p")
  
  combinedStats <- sumStats %>% select(-change_p) %>% 
    pivot_wider(names_from=Photo_MT,values_from = change)  
  
  sumStats$Photo <- unlist(lapply(sumStats$Photo_MT,function(X){
    return(strsplit(X,"_")[[1]][1])
  }))
  
  sumStats$Mycorrhizal.type <- unlist(lapply(sumStats$Photo_MT,function(X){
    return(strsplit(X,"_")[[1]][2])
  }))
  
  mdl <- lm(change~Mycorrhizal.type + Photo,data=sumStats)
  sum <- summary(mdl)
  
  coefDat <- data.frame(typ=c('Mycorrhizal Type','Photo Sensitivity'),
                        coeff = abs(sum$coefficients[2:3,1]),
                        sd=sum$coefficients[2:3,2])
  
  newP<- ggplot(coefDat, aes(x=typ, y=coeff)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=coeff-sd, ymax=coeff+sd), width=.2,
                  position=position_dodge(.9)) +
    labs(title=phenoDescription, x="Variable", y = "Effect on Change")+
    theme_classic()
  allP[[p]] <- newP

}

jpeg(filename='comparingPhotoAndMTeffects.jpeg',width=7,height=7,units="in",res=1000)
do.call(grid.arrange,allP)
dev.off()

includedStations = data.frame(PEP_ID=includedStations)
includedStations <- left_join(includedStations,stationDat,by="PEP_ID")
write.csv(includedStations,file="PEPlongtermStations.csv",quote=FALSE,row.names = FALSE)
