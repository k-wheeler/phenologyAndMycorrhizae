library(rjags)
library(runjags)
source('sharedVariables.R')
source('runMCMC_Model.R')
set.seed(0)

#Load and Organize Data ----
p=3
load(file=paste0("allCombinedNEONDat_",gsub(" ","",NEON_phenophase_names[p]),".RData")) #Loaded as allComDat
allComDat$phenoStatus <- as.numeric(as.character(allComDat$phenoStatus))
selectAllComDat <- allComDat %>% filter(!is.na(soilMoisture),siteID=="HARV")

subSelectAllComDat <- selectAllComDat[sample.int(nrow(selectAllComDat),10000,replace=FALSE),]
allSunlightTimes <- read.csv(file=paste0(dataPath,'NEON_sunlightTimes.csv'))
siteMaxDaylength <- allSunlightTimes %>% group_by(siteID) %>% 
  summarize(maxDayLength=max(daylength))
subSelectAllComDat <- left_join(subSelectAllComDat,siteMaxDaylength,by="siteID") %>%
  mutate(pRatio=daylength/maxDayLength)

subSelectAllComDat$CDDp <- subSelectAllComDat$CDD_closest * subSelectAllComDat$pRatio

drought <- cbind(subSelectAllComDat$scientificName,subSelectAllComDat$soilMoisture)
colnames(drought) <- c("scientificName","drought")
drought=as.data.frame(drought) %>% group_by(scientificName) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=drought)
drought$scientificName <- NULL

CDDp <- cbind(subSelectAllComDat$scientificName,subSelectAllComDat$CDDp)
colnames(CDDp) <- c("scientificName","CDDp")
CDDp=as.data.frame(CDDp) %>% group_by(scientificName) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=CDDp)
CDDp$scientificName <- NULL

phenoStatus <- cbind(subSelectAllComDat$scientificName,subSelectAllComDat$phenoStatus)
colnames(phenoStatus) <- c("scientificName","phenoStatus")
numObs <- as.data.frame(phenoStatus) %>% group_by(scientificName) %>% summarize(n=n())

phenoStatus=as.data.frame(phenoStatus) %>% group_by(scientificName) %>% mutate(rowNum=row_number()) %>%
  pivot_wider(names_from=rowNum,values_from=phenoStatus)
numObs <- left_join(phenoStatus,numObs,by="scientificName")
numObs <- numObs$n

phenoStatus$scientificName <- NULL

JAGSdat <- list(drought=matrix(as.numeric(as.matrix(drought)),ncol=ncol(drought)),
                CDDp=matrix(as.numeric(as.matrix(CDDp)),ncol=ncol(CDDp)),
                phenoStatus=matrix(as.numeric(as.matrix(phenoStatus)),ncol=ncol(phenoStatus)),
                N=length(numObs),
                numObs=numObs
                )

yCritPriorValues <- read.csv('archettiYcritValues.csv')
#plot(density(yCritPriorValues$Ycrit))
priorMean=mean(yCritPriorValues$Ycrit)
priorVar=var(yCritPriorValues$Ycrit)
JAGSdat$CDD_mean_shape=priorMean**2/priorVar #k in JAGS
JAGSdat$CDD_mean_scale=priorVar/priorMean #theta in JAGS
JAGSdat$CDD_tau_lower <- 0
JAGSdat$CDD_tau_upper <- 1/(priorVar*30) #inflated by a lot because of limited range of prior data


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
CDDCrit[i] ~ dnorm(CDDCrit_mean,CDDCrit_tau) #Random effect
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

CDDCrit_mean ~ dgamma(CDD_mean_shape,CDD_mean_scale)
CDDCrit_tau ~ dunif(CDD_tau_lower,CDD_tau_upper)

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
variableNames <- c("CDDCrit","CDDCrit_mean","CDDCrit_tau","p")
# out.burn <- coda.samples(model=j.model,variable.names=variableNames,n.iter=20000)
out.burn <- runMCMC_Model(j.model=j.model,variableNames=variableNames,
                          baseNum = 10000,iterSize = 5000)
save(out.burn,file="DM_HARV_JAGS_varBurn.RData")
out.mat <- as.data.frame(as.matrix(out.burn))
# 
# pdf(file="test.pdf",
#     width=6,height=6)
# plot(density(out.mat$k_mean))
# plot(density(out.mat$k_tau))
# plot(density(out.mat$p))
# plot(density(out.mat$'k[1]'))
# plot(density(out.mat$'k[10]'))
# plot(density(out.mat$'k[20]'))
# plot(density(out.mat$'k[30]'))
# plot(density(out.mat$'k[40]'))
# plot(density(out.mat$'k[51]'))
# plot(density(out.mat$'k[61]'))
# plot(density(out.mat$'k[81]'))
# plot(density(out.mat$'k[101]'))
# plot(density(out.mat$'k[121]'))
# plot(density(out.mat$'k[141]'))
# plot(density(out.mat$'k[161]'))
# plot(density(out.mat$'k[181]'))
# plot(density(out.mat$'k[201]'))
# plot(density(out.mat$'k[241]'))
# plot(density(out.mat$'k[261]'))
# plot(density(out.mat$'k[301]'))
# dev.off()