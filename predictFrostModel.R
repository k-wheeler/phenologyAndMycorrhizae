#Model to Predict Last Spring and First Autumn Frost
library('rjags')
library('runjags')
library('tidyverse')
source('runMCMC_Model.R')

load('frostModelDataObject_spring.RData') #Loaded as springData
# N <- 10000
# samples <- sample.int(length(springData$D),size=N,replace = FALSE)
# 
# for(i in seq(1,length(springData))){
#   springData[[i]] <- springData[[i]][samples]
# }
springData$N <- length(springData$D)


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
  for(i in 1:N) {
    logit(p[i]) <- b0 + b_GDD * GDD[i] + b_D * D[i] + b_P * P[i]
    y[i] ~ dbern(p[i])
  }
  # Priors
  p0 ~ dbeta(1, 1)
  b0 <- logit(p0)
  b_GDD ~ dunif(-5, 5)
  b_D ~ dunif(-5, 5)
  b_P ~ dunif(-5, 5)
}
"

j.model <- jags.model(file=textConnection(frostModel),
                      data=springData,
                      n.chains=3)
variableNames <- c('b0','b_GDD','b_D','b_P')
#test=coda.samples(j.model,variable.names = variableNames,n.iter=5000)

jags.out <- runMCMC_Model(j.model,variableNames = variableNames,baseNum = 10000,iterSize = 5000)
save(jags.out,file="springFrostPredictionVarburn_full.RData")

#jags.mat <- data.frame(as.matrix(jags.out))
