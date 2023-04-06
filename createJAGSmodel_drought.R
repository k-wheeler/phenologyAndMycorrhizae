library(rjags)
library(runjags)
source('sharedVariables.R')
source('runMCMC_Model.R')
library(caTools)
library('xgboost')
library(randomForest)
library('mgcv')
set.seed(0)

#Load and Organize Data ----
p=3
nchain=5

load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat
allComDat$phenoStatus <- as.numeric(as.character(allComDat$phenoStatus))
selectAllComDat <- allComDat %>% filter(!is.na(soilMoisture),siteID=="HARV",!is.na(CDDp))

subSelectAllComDat <- selectAllComDat

siteIDs <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$siteID)
colnames(siteIDs) <- c("individualID","siteIDs")
siteIDs=as.data.frame(siteIDs) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=siteIDs)
siteIDs$individualID <- NULL

drought <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$soilMoisture)
colnames(drought) <- c("individualID","drought")
drought=as.data.frame(drought) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=drought)
drought$individualID <- NULL

dates <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$date)
colnames(dates) <- c("individualID","dates")
dates=as.data.frame(dates) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=dates)
dates$individualID <- NULL

daylength <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$daylength)
colnames(daylength) <- c("individualID","daylength")
daylength=as.data.frame(daylength) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=daylength)
daylength$individualID <- NULL


CDDp <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$CDDp)
colnames(CDDp) <- c("individualID","CDDp")
CDDp=as.data.frame(CDDp) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=CDDp)
CDDp$individualID <- NULL

phenoStatus <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$phenoStatus)
colnames(phenoStatus) <- c("individualID","phenoStatus")
numObs <- as.data.frame(phenoStatus) %>% group_by(individualID) %>% summarize(n=n())

phenoStatus=as.data.frame(phenoStatus) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=phenoStatus)
numObs <- left_join(phenoStatus,numObs,by="individualID")
numObs <- numObs$n

phenoStatus$individualID <- NULL
includeRow <- logical()
for(i in 1:nrow(phenoStatus)){
  print(sum(na.omit(as.numeric(as.character(phenoStatus[i,])))==1))
  includeRow <- c(includeRow,
                  (sum(na.omit(as.numeric(as.character(phenoStatus[i,])))==1)>100))
}
# sampleRow <- sample(which(includeRow),20,replace=FALSE)
# includeRow <- rep(FALSE,length(includeRow))
# includeRow[sampleRow] <- TRUE

siteIDs=(matrix(as.numeric(as.matrix(siteIDs)),ncol=ncol(siteIDs))[includeRow,])
drought=(matrix(as.numeric(as.matrix(drought)),ncol=ncol(drought))[includeRow,])
CDDp=matrix(as.numeric(as.matrix(CDDp)),ncol=ncol(CDDp))[includeRow,]
phenoStatus=matrix(as.numeric(as.matrix(phenoStatus)),ncol=ncol(phenoStatus))[includeRow,]
daylength=(matrix(as.numeric(as.matrix(daylength)),ncol=ncol(daylength))[includeRow,])
dates=(matrix(as.numeric(as.matrix(dates)),ncol=ncol(drought))[includeRow,])

JAGSdat <- list(drought=drought,
                CDDp=CDDp,
                phenoStatus=phenoStatus,
                N=sum(includeRow),
                numObs=numObs[includeRow],
                daylength=daylength,
                siteID=siteIDs
                )

yCritPriorValues <- read.csv('archettiYcritValues.csv')
#plot(density(yCritPriorValues$Ycrit))
priorMean=mean(yCritPriorValues$Ycrit)
priorVar=var(yCritPriorValues$Ycrit) #Inflate variance for out of sample
JAGSdat$CDD_mean_shape=priorMean/priorVar #k in JAGS
JAGSdat$CDD_mean_rate=(priorMean**2)/priorVar #theta in JAGS

# #Investigating why There seems to be multiple modes in the CDDp values ----
# CDDp <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$CDD_closest)
# colnames(CDDp) <- c("individualID","CDDp")
# CDDp=as.data.frame(CDDp) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
#   pivot_wider(names_from=rowNum,values_from=CDDp)
# speciesToTest <- CDDp$individualID[includeRow]
# 
# subSelectAllComDat <- selectAllComDat %>% dplyr::select(siteID,year,individualID,canopyPosition,height,
#                                                         daylength,drought,averageDrought,springDayOfYear,
#                                                         soilMoisture,CDD_closest,individualID,phenoStatus) %>%
#   filter(!is.na(year),!is.na(individualID),!is.na(canopyPosition),!is.na(height),
#          !is.na(daylength),!is.na(drought),!is.na(averageDrought),!is.na(springDayOfYear),
#          !is.na(soilMoisture),!is.na(CDD_closest),!is.na(individualID))
# j <- which(names(subSelectAllComDat)=="CDD_closest")
# pdf(file="speciesCDD_rf3.pdf",
#     width=6,height=6)
# for(s in seq_along(speciesToTest)){
#   subDat <- subSelectAllComDat %>% filter(individualID==speciesToTest[s],phenoStatus==1)
#   if(nrow(subDat)>5){
#     print(s)
#     rf <- randomForest(CDD_closest ~ ., data=subDat)
#     randomForest::varImpPlot(rf)
#     plot(subDat$soilMoisture,subDat$CDD_closest,pch=20,main=speciesToTest[s])
#     partialPlot(x=rf,pred.data=data.frame(subDat),x.var="daylength")
#     partialPlot(x=rf,pred.data=data.frame(subDat),x.var="soilMoisture")
#   }
# }
# dev.off()
# 
# 
# 
# subDat <- subSelectAllComDat %>% filter(phenoStatus==1)
# 
# rf <- randomForest(CDD_closest ~ ., data=subDat)
# 
# pdf(file="CDD_rf.pdf",
#     width=6,height=6)
# randomForest::varImpPlot(rf)
# #plot(subDat$soilMoisture,subDat$CDD_closest,pch=20,main=speciesToTest[s])
# partialPlot(x=rf,pred.data=data.frame(subDat),x.var="daylength")
# partialPlot(x=rf,pred.data=data.frame(subDat),x.var="soilMoisture")
# 
# dev.off()


# yesData <- selectAllComDat %>% filter(phenoStatus=="1",drought<1,CDD_10>0)
# #yesData <- selectAllComDat %>% filter(phenoStatus=="1")
# pdf(file="CDDvsDrought.pdf",
#     width=6,height=6)
# plot(yesData$drought,(yesData$CDD_10),pch=20,cex=1)
# points(jitter(yesData$drought[yesData$Mycorrhizal.type=="AM"]),jitter(yesData$CDD_10[yesData$Mycorrhizal.type=="AM"]),pch=20,col="brown")
# points(jitter(yesData$drought[yesData$Mycorrhizal.type=="EcM"]),jitter(yesData$CDD_10[yesData$Mycorrhizal.type=="EcM"]),pch=20,col="cyan")
# dev.off()

#Define Model ----
generalModel = "
model {
#### Process Model
for(i in 1:N){
CDDCrit[i] ~ dgamma(CDD_mean_shape,CDD_mean_rate) #Random effect
#k[i] ~ dnorm(k_mean,k_tau) #Random effect

for(j in 1:numObs[i]){    
phenoProb[i,j] <- ifelse(CDDp[i,j]>CDDCrit[i],p,(1-p)) #1 for senescence and 0 for no
phenoStatus[i,j] ~ dbern(phenoProb[i,j]) 
#CDDCrit[i,j] <- exp(k[i]*(drought[i,j]))
}
}

#### Priors
#k_mean ~ dunif(0.01,10)
#k_tau ~ dunif(0.1,50)
#CDDmean ~ dunif(0,1000)
#tau ~ dunif(0,0.01)
#CDDCrit ~dunif(0,400)

p ~ dbeta(10,1) T(0.5,1)
}
"

inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(p=rnorm(1,0.9,0.001),CDDCrit=rnorm(JAGSdat$N,priorMean,10))
}
# for(i in 1:nchain){
#   inits[[i]] <- list(p=rnorm(1,0.9,0.001),k=rnorm(1,-1,0.1))
# }

#Run Model ----
j.model   <- jags.model(file = textConnection(generalModel),
                        data = JAGSdat,
                        n.chains = nchain,
                        inits=inits)
#variableNames <- c("p.proc","b0","b1","x")
#variableNames <- c("CDDCrit_mean","CDDCrit_tau","p")
variableNames <- c("CDDCrit","p")
# 
var.out   <- coda.samples (model = j.model,
                           variable.names = variableNames,
                           n.iter = 2000)
gelman.diag(var.out)

pdf(file="fuzzyCaterpillers3_new.pdf",
    width=6,height=6)
plot(var.out)
dev.off()
var.burn=var.out

# out.burn <- coda.samples(model=j.model,variable.names=variableNames,n.iter=20000)
# out.burn <- runMCMC_Model(j.model=j.model,variableNames=variableNames,
#                           baseNum = 10000,iterSize = 10000,maxGBR=30)
save(var.burn,file="DM_HARV_JAGS_varBurn_sub.RData")
# load(file="DM_HARV_JAGS_varBurn.RData")
# out.mat <- as.data.frame(as.matrix(out.burn))

# pdf(file="DM_HARV_parameterDensities.pdf",
#     width=6,height=6)
# plot(density(out.mat$p))
# plot(density(out.mat$'CDDCrit[1]'))
# plot(density(out.mat$'CDDCrit[2]'))
# plot(density(out.mat$'CDDCrit[3]'))
# plot(density(out.mat$'CDDCrit[4]'))
# plot(density(out.mat$'CDDCrit[5]'))
# plot(density(out.mat$'CDDCrit[6]'))
# plot(density(out.mat$'CDDCrit[7]'))
# plot(density(out.mat$'CDDCrit[8]'))
# plot(density(out.mat$'CDDCrit[9]'))
# plot(density(out.mat$'CDDCrit[10]'))
# plot(density(out.mat$'CDDCrit[11]'))
# 
# # plot(density(out.mat$CDDCrit_mean))
# # plot(density(out.mat$CDDCrit_tau))
# # plot(density(out.mat$p))
# # plot(density(out.mat$'CDDCrit[1]'))
# # plot(density(out.mat$'CDDCrit[2]'))
# # plot(density(out.mat$'CDDCrit[5]'))
# 
# dev.off()
# 
# pdf(file="CDDrelationships_individual.pdf",
#     width=6,height=6)
# for(i in 1:nrow(JAGSdat$phenoStatus)){
#   print(i)
#   ids <- which(JAGSdat$phenoStatus[i,]==1)
#   print(paste0("length yes:",length(na.omit(JAGSdat$CDDp[i,-ids]))))
#   print(paste0("length no:",length(ids)))
#   if(length(ids)>1){
#     plot(density(na.omit(JAGSdat$CDDp[i,-ids])),xlim=range(JAGSdat$CDDp[i,],na.rm=TRUE))
#     lines(density(na.omit(JAGSdat$CDDp[i,ids])),col="orange")
#     
#     plot(density(na.omit(JAGSdat$drought[i,-ids])),xlim=range(JAGSdat$drought[i,],na.rm=TRUE))
#     lines(density(na.omit(JAGSdat$drought[i,ids])),col="orange")
#     
#     plot(density(na.omit(JAGSdat$daylength[i,-ids])),xlim=range(JAGSdat$daylength[i,],na.rm=TRUE))
#     lines(density(na.omit(JAGSdat$daylength[i,ids])),col="orange")
#   }
# }
# dev.off()


