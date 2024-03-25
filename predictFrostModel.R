#Model to Predict Last Spring and First Autumn Frost
library('rjags')
library('runjags')
library('tidyverse')
source('runMCMC_Model.R')

season <- "spring"
load(paste0('frostModelDataObject_',season,'.RData')) #Loaded as seasonData

seasonData$N <- nrow(seasonData$D)
seasonData$regionN <- ncol(seasonData$D)

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
    b_GDD[j] ~ dnorm(b_GDD_mean,b_GDD_prec)
    b_D[j] ~ dnorm(b_D_mean,b_D_prec)
    b_P[j] ~ dnorm(b_P_mean,b_P_prec)
    
    for(i in 1:N) {
      logit(p[i,j]) <- b_0[j] + b_GDD[j] * GDD[i,j] + b_D[j] * D[i,j] + b_P[j] * P[i,j]
      y[i,j] ~ dbern(p[i,j])
    }
  }
  # Priors
  p0 ~ dbeta(1, 1)
  b_0_mean <- logit(p0)
  b_GDD_mean ~ dunif(-5, 5)
  b_D_mean ~ dunif(-5, 5)
  b_P_mean ~ dunif(-5, 5)
  
  #Precisions on Random Effects
  b_0_prec ~ dunif(0.00000001,0.1)
  b_GDD_prec ~ dunif(0.00000001,0.1)
  b_D_prec ~ dunif(0.00000001,0.1)
  b_P_prec ~ dunif(0.00000001,0.1)
}
"

j.model <- jags.model(file=textConnection(frostModel),
                      data=seasonData,
                      n.chains=3)
variableNames <- c('b_0','b_GDD','b_D','b_P','b_0_mean','b_GDD_mean','b_D_mean','b_P_mean','b_0_prec','b_GDD_prec','b_D_prec','b_P_prec')
#variableNames <- c('b_0_mean','b_GDD_mean','b_D_mean','b_P_mean')#,'b_0_prec','b_GDD_prec','b_D_prec','b_P_prec')
#test=coda.samples(j.model,variable.names = variableNames,n.iter=500)

jags.out <- runMCMC_Model(j.model,variableNames = variableNames,baseNum = 10000,iterSize = 2000)
save(jags.out,file="springFrostPredictionVarburn_hierarchical.RData")

#jags.mat <- data.frame(as.matrix(jags.out))
