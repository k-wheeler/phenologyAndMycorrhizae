source('sharedVariables.R')
dataFilePath <- "Data/NEON_presence-cover-plant-CLEAN/"

files <- dir(path=dataFilePath)
library(RColorBrewer)
coul <- c(brewer.pal(name="Dark2", n = 8),brewer.pal(6, "Pastel2"))

mycorrhizeTypes[12] <- "Species-Specific"

#mycorrhizaeCovers <- matrix(nrow=length_NEON_siteNames,ncol=(length(mycorrhizeTypes)))
mycorrhizaeCovers <- matrix(nrow=length(files),ncol=(length(mycorrhizeTypes)+2))
mycorrhizaeCovers <- as.data.frame(mycorrhizaeCovers)
colnames(mycorrhizaeCovers) <- c("siteID","date",mycorrhizeTypes)

#for(s in seq_along(NEON_siteNames)){
for(f in seq_along(files)){
  dat <- read.csv(paste0(dataFilePath,files[f]))
  if(sum(!is.na(dat$divDataType))>0){
    dat <- dat %>% filter(divDataType=="plantSpecies") %>% select(c('scientificName','percentCover'))
    dat$Genus <- sapply(strsplit(as.character(dat$scientificName)," "),`[`,1)
    dat <- left_join(dat,fungalRootDat,by="Genus")
    for(i in seq_along(mycorrhizeTypes)){
      mycorrhizaeCovers[f,(i+2)] <- sum(dat[(dat$'Mycorrhizal.type')==mycorrhizeTypes[i],'percentCover'],na.rm=TRUE)
    }
  }
  mycorrhizaeCovers$siteID[f] <- strsplit(files[f],"[.]")[[1]][3]
  mycorrhizaeCovers$date[f] <- strsplit(files[f],"[.]")[[1]][8]
}

mycorrhizaeCovers[,3:ncol(mycorrhizaeCovers)] <- t(apply(as.data.frame(lapply(mycorrhizaeCovers[,3:ncol(mycorrhizaeCovers)],as.numeric)),1,function(x) x/sum(na.omit(x)))*100)
mycorrhizaeCovers$date <- as.Date(paste(mycorrhizaeCovers$date,"-01",sep=""))
write.csv(file="NEON_site_cover_MTclassifications.csv",mycorrhizaeCovers,row.names=FALSE,quote = FALSE)

mycorrhizaeBiomass <- read.csv("NEON_site_treeBiomass_MTclassifications.csv")
pdf("NEON_site_MT_classifications.pdf",height=5,width=12)
par(mfrow=c(1,3))
for(siteID in NEON_siteNames){
  print(siteID)
  #siteID <- "BART"
  siteDat <- mycorrhizaeCovers[mycorrhizaeCovers$siteID==siteID,]
  row.names(siteDat) <- siteDat$date
  
  #plot(test$date,test$AM,type="l")
  
  
  barplot(t(siteDat[,3:ncol(siteDat)]),col=coul,ylim=c(0,100),las=3,cex.names = 0.5,main=siteID,xlab="Measurement Month",ylab="All Cover Percent")
  siteDat <- mycorrhizaeBiomass[mycorrhizaeBiomass$siteID==siteID,]
  row.names(siteDat) <- siteDat$date
  if(nrow(siteDat)>0){
  barplot(t(siteDat[,3:ncol(siteDat)]),col=coul,ylim=c(0,1),las=3,cex.names = 0.5,main=siteID,xlab="Measurement Month",ylab="Tree Biomass Ratio")
  }else{
    plot.new()
  }
  plot.new()
  legend("topleft",legend=mycorrhizeTypes[-c(9,10,13,14)],col=coul[-c(9,10,13,14)],lty=1,lwd=10,cex=0.8,bty="n")
}
dev.off()


