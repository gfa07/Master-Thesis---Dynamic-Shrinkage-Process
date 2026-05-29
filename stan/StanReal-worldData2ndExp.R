library('cmdstanr')

args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])

prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")

load("dataPraticalExp.Rda")
responseVar = c("IBM","DELTA", "MOBIL")

ff = read.csv("F-F_Research_Data_Factors-SMB_HML.csv")
SMB = ff[ ,2]
HML = ff[ ,3]


y = data[[responseVar[i]]] - data$RKFREE #Just change the variable
x = data$MARKET - data$RKFREE
X = cbind(Intercept = 1, ExcessMarket = x, SMB, HML)

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


fit$save_object(paste0("fit_", responseVar[i], "_NonCentSV2Exp.rds")) #The name depends on the variable selected!