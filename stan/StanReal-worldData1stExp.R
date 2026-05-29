library('cmdstanr')
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")

load("dataPraticalExp.Rda")
y = data$MOBIL - data$RKFREE #Just change the variable
x = data$MARKET - data$RKFREE
X = cbind(1,x)

phiPrior = c(10, 2)
xiPrior = c(-1.683977, 0.7891323) # To approximate the PG effect

#SV
muPriorSV = c(0, 100)
phiTransfPriorSV = c(5, 1.5)
varianceEtaPriorSV = c(0.5, 0.5)

stanData <- list(T=nrow(X), P=ncol(X), X=X, y=y, alpha2 = 0.5, beta2 = 0.5,
                 phiPriorTransf = phiPrior,
                 xiPrior = xiPrior,
                 muPriorSV = muPriorSV,
                 phiTransfPriorSV = phiTransfPriorSV,
                 varianceEtaPriorSV = varianceEtaPriorSV)


burnin <- 1000
niter <- 13000


model <- cmdstan_model("TVRegDSPmodelSeparateHyperFullNonCentSV.stan")


fit = model$sample(data = stanData,
                   chains = 1,
                   iter_warmup = burnin,
                   iter_sampling = niter - burnin,
                   seed = 123 ,
                   adapt_delta = 0.995,
                   max_treedepth = 15)


fit$save_object("fit_MobilDataNonCentSV.rds") #The name depends on the variable selected!
