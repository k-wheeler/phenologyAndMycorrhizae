source('sharedVariables.R')
source('downloadPhenoCam.R')
source('calculatePhenoCamUncertainty.R')
options(stringsAsFactors = FALSE)

siteData$topPhenocam <- sapply(strsplit(siteData$field_phenocams,"[()]"),`[`,2)
siteData$bottomPhenocam <- sapply(strsplit(siteData$field_phenocams,"[()]"),`[`,4)
siteData$phenocam_code <- sapply(strsplit(siteData$topPhenocam,"/"),`[`,6)
siteData$phenocam_code_bottom <- sapply(strsplit(siteData$bottomPhenocam,"/"),`[`,6) ###########
potentialPFTs <- c("AG","DB","EB","EN","DN","GR","MX","SH","TN","UN","WL")
potentialROInumbers <- c("1000","2000","1001","1002")
potentialROIs <- expand.grid(potentialPFTs,potentialROInumbers)
potentialROIs <- paste(potentialROIs$Var1,potentialROIs$Var2,sep="_")
potentialROIs_understory <- paste("UN",potentialROInumbers,sep="_")

phenoCamURLs <- matrix(ncol=6,nrow=0)

for(s in 2:nrow(siteData)){
  siteName <-  siteData$phenocam_code_bottom[s]
  print(siteName)
  for(r in seq_along(potentialROIs_understory)){
    URL_gcc90 <- paste('https://phenocam.nau.edu/data/archive/',siteName,"/ROI/",siteName,"_",potentialROIs[r],"_1day.csv",sep="") 
    if(valid_url(URL_gcc90)){
      print(potentialROIs[r])
      phenoCamURLs <- rbind(phenoCamURLs,c(siteData$field_site_id[s],
                                           siteName,
                                           URL_gcc90,
                                           siteData$field_latitude[s],
                                           siteData$field_longitude[s],
                                           potentialROIs[r]))
    }
  }
}
phenoCamURLs <- data.frame(phenoCamURLs)
colnames(phenoCamURLs) <- c('site_id','phenocam_id','URL_gcc90','latitude','longitude','ROI')
write.csv(phenoCamURLs,file=paste0(dataPath,'phenocamURLS.csv'),row.names=FALSE,quote=FALSE)
