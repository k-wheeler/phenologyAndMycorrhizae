#Model to Predict Last Spring and First Autumn Frost
library('rjags')
library('runjags')
library('tidyverse')
source('runMCMC_Model.R')

season <- "fall"
load(paste0('frostModelDataObject_',season,'.RData')) #Loaded as seasonData
ecoregionIDs <- read.table('ecoregionIDs.txt',sep=";",header=TRUE)
seasonData$N <- nrow(seasonData$D)
seasonData$regionN <- ncol(seasonData$D)
ecoIDs <- data.frame(ECO_ID=as.numeric(colnames(seasonData$D)))
ecoIDs <- left_join(ecoIDs,ecoregionIDs,by='ECO_ID')
ecoIDs <- ecoIDs %>% dplyr::select(ECO_ID,ECO_NAME)
ecoIDs$region <- as.character(row_number(ecoIDs$ECO_ID))
if(season=="spring"){
  seasonData$DD <- seasonData$GDD
}else if(season=="fall"){
  seasonData$DD <- seasonData$CDD
}

# Cumulative precipitation
#CDD base 20 over a moving window 14 days prior
#Sum of chilling hours over a moving window 135 to 45 days prior (exclude for now)
#GDD base 0 over a moving window 45 days to one day prior
#Day length of prior day

#Each site has a probability of being post last spring frost or before first autumn frost

#logit(p) <- X * b

#Based off of Elmendorf et al. (2019)

frostModel <- "
model{
  # Likelihood
  for(j in 1:regionN){
    #Region Random Effects
    b_0[j] ~ dnorm(b_0_mean,b_0_prec)
    b_DD[j] ~ dnorm(b_DD_mean,b_DD_prec)
    b_D[j] ~ dnorm(b_D_mean,b_D_prec)
    b_P[j] ~ dnorm(b_P_mean,b_P_prec)
    
    for(i in 1:N) {
      logit(p[i,j]) <- b_0[j] + b_DD[j] * DD[i,j] + b_D[j] * D[i,j] + b_P[j] * P[i,j]
      y[i,j] ~ dbern(p[i,j])
    }
  }
  # Priors
  p0 ~ dbeta(1, 1)
  b_0_mean <- logit(p0)
  b_DD_mean ~ dunif(-5, 5)
  b_D_mean ~ dunif(-5, 5)
  b_P_mean ~ dunif(-5, 5)
  
  #Precisions on Random Effects
  b_0_prec ~ dunif(0.00000001,0.1)
  b_DD_prec ~ dunif(0.00000001,0.1)
  b_D_prec ~ dunif(0.00000001,0.1)
  b_P_prec ~ dunif(0.00000001,0.1)
}
"

j.model <- jags.model(file=textConnection(frostModel),
                      data=seasonData,
                      n.chains=3)
variableNames <- c('b_0','b_DD','b_D','b_P','b_0_mean','b_DD_mean','b_D_mean','b_P_mean','b_0_prec','b_DD_prec','b_D_prec','b_P_prec')
#variableNames <- c('b_0_mean','b_GDD_mean','b_D_mean','b_P_mean')#,'b_0_prec','b_GDD_prec','b_D_prec','b_P_prec')
#test=coda.samples(j.model,variable.names = variableNames,n.iter=500)

jags.out <- runMCMC_Model(j.model,variableNames = variableNames,baseNum = 100000,iterSize = 10000,sampleCutoff=3000)

out.mat <- as.matrix(jags.out)
thinAmount <- round(nrow(out.mat)/5000,digits=0)

jags.out <- window(jags.out,thin=thinAmount)

save(jags.out,file=paste0(season,"FrostPredictionVarburn_hierarchical.RData"))

# jags.mat <- data.frame(data.frame(as.matrix(jags.out)))
# summary(jags.mat)
# 
# b_D_cols = grep("b_D",colnames(jags.mat),fixed=TRUE)
# b_D <- jags.mat[,b_D_cols]
# b_D_sum <- data.frame(region=names(b_D),mn=colMeans(b_D),sd=apply(b_D,MARGIN = 2,FUN=sd),driver="D")
# b_D_sum$region <- c(seq(1,16),"mean","prec")
# 
# b_GDD_cols = grep("b_GDD",colnames(jags.mat),fixed=TRUE)
# b_GDD <- jags.mat[,b_GDD_cols]
# b_GDD_sum <- data.frame(region=names(b_GDD),mn=colMeans(b_GDD),sd=apply(b_GDD,MARGIN = 2,FUN=sd),driver="GDD")
# b_GDD_sum$region <- c(seq(1,16),"mean","prec")
# 
# allSum <- rbind(b_D_sum,b_GDD_sum)
# 
# allSum <- left_join(allSum,ecoIDs,by="region")
# allSum$ECO_NAME[allSum$region=="mean"] <- "Overall Mean"
# allSum$ECO_NAME[allSum$region=="prec"] <- "Effect Precision"
# 
# 
# pdf('springFrostOutput.pdf',height=12,width=8)
# # ggplot(data=b_D_sum,aes(x=region,y=mn)) + 
# #   geom_bar(stat="identity")+
# #   geom_errorbar(aes(ymin=mn-sd,ymax=mn+sd),width=0.2,position=position_dodge(0.9))+
# #   theme_minimal()
# # ggplot(data=b_GDD_sum,aes(x=region,y=mn)) + 
# #   geom_bar(stat="identity")+
# #   geom_errorbar(aes(ymin=mn-sd,ymax=mn+sd),width=0.2,position=position_dodge(0.9))+
# #   theme_minimal()
# ggplot(data=allSum,aes(x=ECO_NAME,y=mn,fill=driver)) + 
#   geom_bar(stat="identity",position=position_dodge())+
#   geom_errorbar(aes(ymin=mn-sd,ymax=mn+sd),width=0.2,position=position_dodge(0.9))+
#   theme_minimal() + coord_flip()
# 
# dev.off()



