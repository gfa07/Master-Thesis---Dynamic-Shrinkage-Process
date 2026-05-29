#Note that this code was formulated for the Tetralith 
library('cmdstanr')
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")

args <- commandArgs(trailingOnly = TRUE)
delta <- as.numeric(args[1])

#Model for Synthetic Data

set.seed(123)
T <- 500
P <- 3

betas <- matrix(NA, nrow = T, ncol = P)
for (t in 1:T){
  betas[t,1] = sin(10*t/T)
  betas[t,2] = if (t/T < 0.5) -0.5 else 0.5
  betas[t,3] = (t/T)^2
}


X <- matrix(rnorm(T*(P-1), mean = 0, sd = 1), nrow = T, ncol = P-1)
X <- cbind('Intercept' = 1, X)

SDSig <- 0.45

y <- numeric(T)
for (t in 1:T){
  y[t] <- sum(X[t, ]*betas[t, ]) + rnorm(1, mean = 0, sd = SDSig)
}

phiPrior = c(10, 2)
sigmaEpsPrior = c(0.00001, 0.00001)
xiPrior = c(-1.683977, 0.7891323) # To approximate the PG effect

stanData <- list(T=nrow(X), P=ncol(X), X=X, y=y, alpha2 = 0.5, beta2 = 0.5,
                 phiPriorTransf = phiPrior,
                 sigmaEpsPrior = sigmaEpsPrior,
                 xiPrior = xiPrior)

burnin <- 1000
niter <- 13000

model = cmdstan_model("TVRegDSPmodelSeparateHyperFullNonCent.stan")


fit = model$sample(data = stanData,
                     chains = 1,
                     iter_warmup = burnin,
                     iter_sampling = niter - burnin,
                     seed = 123 ,
                     adapt_delta = delta,
                     max_treedepth = 15)

  
fit$save_object(paste0("fitCmdstanr", delta, "w1chains.rds"))


