library(rjags)
library(runjags)
source('sharedVariables.R')
source('runMCMC_Model.R')
set.seed(0)


#Load and Organize Data ----
p=3
load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat
allComDat$phenoStatus <- as.numeric(as.character(allComDat$phenoStatus))
selectAllComDat <- allComDat %>% filter(Mycorrhizal.type%in%c("AM","EcM"),!is.na(soilMoisture))

subSelectAllComDat <- selectAllComDat[sample.int(nrow(selectAllComDat),10000,replace=FALSE),]

drought <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$soilMoisture)
colnames(drought) <- c("individualID","drought")
drought=as.data.frame(drought) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=drought)
drought$individualID <- NULL

CDD <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$CDD_10)
colnames(CDD) <- c("individualID","CDD")
CDD=as.data.frame(CDD) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=CDD)
CDD$individualID <- NULL

phenoStatus <- cbind(subSelectAllComDat$individualID,subSelectAllComDat$phenoStatus)
colnames(phenoStatus) <- c("individualID","phenoStatus")
numObs <- as.data.frame(phenoStatus) %>% group_by(individualID) %>% summarize(n=n())

phenoStatus=as.data.frame(phenoStatus) %>% group_by(individualID) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=phenoStatus)
numObs <- left_join(phenoStatus,numObs,by="individualID")
numObs <- numObs$n

phenoStatus$individualID <- NULL

JAGSdat <- list(drought=matrix(as.numeric(as.matrix(drought)),ncol=ncol(drought))[numObs>10,],
                CDD=matrix(as.numeric(as.matrix(CDD)),ncol=ncol(CDD))[numObs>10,],
                phenoStatus=matrix(as.numeric(as.matrix(phenoStatus)),ncol=ncol(phenoStatus))[numObs>10,],
                N=length(numObs[numObs>10]),
                numObs=numObs[numObs>10]
                )

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
#CDDCrit[i] ~ dnorm(CDDCrit_mean,CDDCrit_tau) #Random effect
k[i] ~ dnorm(k_mean,k_tau) #Random effect
for(j in 1:numObs[i]){    
phenoProb[i,j] <- ifelse(CDD[i,j]>CDDCrit[i,j],p,(1-p)) #1 for senescence and 0 for no
phenoStatus[i,j] ~ dbern(phenoProb[i,j]) 
CDDCrit[i,j] <- exp(k[i]*(drought[i,j]))
}
}

#### Priors
k_mean ~ dunif(0.01,10)
k_tau ~ dunif(0.1,50)
#CDDmean ~ dunif(0,1000)
#tau ~ dunif(0,0.01)
#CDDCrit ~dunif(0,400)
#CDDCrit_mean ~ dunif(0,400)
#CDDCrit_tau ~ dunif(0,0.00001)
p ~ dbeta(10,1)
}
"

#Run Model ----
nchain=5
j.model   <- jags.model(file = textConnection(generalModel),
                        data = JAGSdat,
                        n.chains = nchain)
#variableNames <- c("p.proc","b0","b1","x")
#variableNames <- c("CDDCrit_mean","CDDCrit_tau","p")
variableNames <- c("k_mean","k_tau","k","p")
# out.burn <- coda.samples(model=j.model,variable.names=variableNames,n.iter=20000)
out.burn <- runMCMC_Model(j.model=j.model,variableNames=variableNames,
                          baseNum = 50000,iterSize = 10000)
save(out.burn,file="SoilMoisture_JAGS_varBurn.RData")
out.mat <- as.data.frame(as.matrix(out.burn))

pdf(file="test.pdf",
    width=6,height=6)
plot(density(out.mat$CDDCrit_mean))
plot(density(out.mat$CDDCrit_tau))
plot(density(out.mat$p))
dev.off()