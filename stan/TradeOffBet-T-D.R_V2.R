setwd("~/Uni/4th semester/RFiles/AlbinoRstanDSP/code")

library('cmdstanr')
library('HDInterval')

set.seed(123)
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#9A84B8",
              "#B5C6DF","#EADAAA","#AE6666","#3A7D7D","#7E6A9E"
              )

######DATA#####
T <- 500
P <- 3

betas <- matrix(NA, nrow = T, ncol = P)
for (t in 1:T){
  betas[t,1] = sin(10*t/T)
  betas[t,2] = if (t/T < 0.5) -0.5 else 0.5
  betas[t,3] = (t/T)^2
}
par(mfrow = c(2,2))
for (j in 1:P){
  plot(betas[,j], type = "l")
}

X <- matrix(rnorm(T*(P-1), mean = 0, sd = 1), nrow = T, ncol = P-1)
X <- cbind('Intercept' = 1, X)

SDSig <- 0.45

y <- numeric(T)
for (t in 1:T){
  y[t] <- sum(X[t, ]*betas[t, ]) + rnorm(1, mean = 0, sd = SDSig)
}


#######Trade-OFF- Delta / Time########
#You will need to generate this models! But there is a fil in this dic that will provide the code for that!
fittedM0.8 = readRDS("stan/fitCmdstanr0.8w1chains.rds")
fittedM0.9 = readRDS("stan/fitCmdstanr0.9w1chains.rds")
fittedM0.95 = readRDS("stan/fitCmdstanr0.95w1chains.rds")
fittedM0.995 = readRDS("stan/fitCmdstanr0.995w1chain.rds")
fittedM0.999999 = readRDS("stan/fitCmdstanr0.999999w1chains.rds")


getTotal_Time_Diver = function(models){
  totalTimes = as.numeric(length(models))
  totalDiver = as.numeric(length(models))
  
  for (j in 1:length(models)) {
    totalTimes[j] = (models[[j]]$time()$total) / 60
    totalDiver[j] = sum(models[[j]]$diagnostic_summary()$num_divergent)
  }
  
  return(list(totalTimes = totalTimes, totalDiver = totalDiver))
}

m = list(fittedM0.8, fittedM0.9, fittedM0.95, fittedM0.995, fittedM0.999999)

totals = getTotal_Time_Diver(m)

namesRow = c('0.8', '0.9', '0.95', '0.995', '0.999999')
data = data.frame(totals, row.names = namesRow)

cairo_pdf("TradeOffRD.pdf", width = 6, height = 7)
par(mfrow = c(2,1))  
barplot(height = data$totalTimes, names.arg = namesRow, col = prettycol[1], ylim = c(0, max(totals$totalTimes*1.2)), xlab = 'adapt_delta', ylab = 'Total Time (minutes)')
barplot(height = data$totalDiver, names.arg = namesRow, col = prettycol[2], ylim = c(0, max(totals$totalDiver*1.2)), xlab = 'adapt_delta', ylab = 'Total Divergences')
dev.off()




#################Compare Betas Posterior########### 

betaDrawsStan0.8 = fittedM0.8$draws(variables = 'betas')
niter = dim(betaDrawsStan0.8)[1] * dim(betaDrawsStan0.8)[2]
betaDrawsStan0.8 = array(betaDrawsStan0.8, dim = c(niter, T, P))

betaDrawsStan0.9 = fittedM0.9$draws(variables = 'betas')
niter = dim(betaDrawsStan0.9)[1]* dim(betaDrawsStan0.9)[2]
betaDrawsStan0.9 = array(betaDrawsStan0.9, dim = c(niter, T, P))

betaDrawsStan0.95 = fittedM0.95$draws(variables = 'betas')
niter = dim(betaDrawsStan0.95)[1]* dim(betaDrawsStan0.95)[2]
betaDrawsStan0.95 = array(betaDrawsStan0.95, dim = c(niter, T, P))

betaDrawsStan0.995 = fittedM0.995$draws(variables = 'betas')
niter = dim(betaDrawsStan0.995)[1]* dim(betaDrawsStan0.995)[2]
betaDrawsStan0.995 = array(betaDrawsStan0.995, dim = c(niter, T, P))

betaDrawsStan0.999999 = fittedM0.999999$draws(variables = 'betas')
niter = dim(betaDrawsStan0.999999)[1]* dim(betaDrawsStan0.999999)[2]
betaDrawsStan0.999999 = array(betaDrawsStan0.999999, dim = c(niter, T, P))

x <- 1:nrow(betas)

PlotBetas <- function(betas, betaDrawsM1, betaDrawsM2, betaDrawsM3, betaDrawsM4, betaDrawsM5, x , j){
  
  medianBetaM1 <- apply(betaDrawsM1, c(2, 3), median)
  hdiM1 = apply(betaDrawsM1, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM2 <- apply(betaDrawsM2, c(2, 3), median)
  hdiM2 = apply(betaDrawsM2, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM3 <- apply(betaDrawsM3, c(2, 3), median)
  hdiM3 = apply(betaDrawsM3, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM4 <- apply(betaDrawsM4, c(2, 3), median)
  hdiM4 = apply(betaDrawsM4, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM5 <- apply(betaDrawsM5, c(2, 3), median)
  hdiM5 = apply(betaDrawsM5, c(2,3), hdi, credMass = 0.95)
  
  lowerlim = min(c(betas[,j], hdiM1[1,,j], hdiM2[1,,j], hdiM3[1,,j], hdiM4[1,,j], hdiM5[1,,j]))
  upperlim = max(c(betas[,j], hdiM1[2,,j], hdiM2[2,,j], hdiM3[2,,j], hdiM4[2,,j], hdiM5[2,,j]))
  
  
  plot(x, betas[,j], type = "l", col = "black", lwd = 2.5, main = bquote(beta[.(j)]), 
       ylab = "", xlab = "time", ylim = c(lowerlim, upperlim))
  
  polygon(c(x, rev(x)), c(hdiM1[1,,j], rev(hdiM1[2,,j])),
          col = adjustcolor(prettycol[6], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiM2[1,,j], rev(hdiM2[2,,j])),
          col = NA, border = prettycol[7])
  
  polygon(c(x, rev(x)), c(hdiM3[1,,j], rev(hdiM3[2,,j])),
          col = NA, border = prettycol[8])
  
  polygon(c(x, rev(x)), c(hdiM4[1,,j], rev(hdiM4[2,,j])),
          col = NA, border = prettycol[9])
  
  polygon(c(x, rev(x)), c(hdiM5[1,,j], rev(hdiM5[2,,j])),
          col = NA, border = prettycol[10])
  
  lines(medianBetaM1[ ,j], col = prettycol[1], lwd = 2)
  
  lines(medianBetaM2[ ,j], col = prettycol[2], lwd = 2)
  
  lines(medianBetaM3[ ,j], col = prettycol[3], lwd = 2)
  
  lines(medianBetaM4[ ,j], col = prettycol[4], lwd = 2)
  
  lines(medianBetaM5[ ,j], col = prettycol[5], lwd = 2)

}



cairo_pdf("beta1Sim_plot.pdf", width = 10, height = 7)
#Betas1
PlotBetas(betas = betas, betaDrawsM1=betaDrawsStan0.8, betaDrawsM2=betaDrawsStan0.9, 
          betaDrawsM3=betaDrawsStan0.95, betaDrawsM4=betaDrawsStan0.995,
          betaDrawsM5=betaDrawsStan0.999999, x = x , j = 1)
dev.off()

cairo_pdf("beta2Sim_plot.pdf", width = 10, height = 7)
#Betas2
PlotBetas(betas = betas, betaDrawsM1=betaDrawsStan0.8, betaDrawsM2=betaDrawsStan0.9, 
          betaDrawsM3=betaDrawsStan0.95, betaDrawsM4=betaDrawsStan0.995,
          betaDrawsM5=betaDrawsStan0.999999, x = x , j = 2)
dev.off()

cairo_pdf("beta3Sim_plot.pdf", width = 10, height = 7)
#Betas3
PlotBetas(betas = betas, betaDrawsM1=betaDrawsStan0.8, betaDrawsM2=betaDrawsStan0.9, 
          betaDrawsM3=betaDrawsStan0.95, betaDrawsM4=betaDrawsStan0.995,
          betaDrawsM5=betaDrawsStan0.999999, x = x , j = 3)
dev.off()


###MU
muDrawsM1 = fittedM0.8$draws(variables = 'mu')
niter = dim(muDrawsM1)[1] * dim(muDrawsM1)[2]
muDrawsM1 = array(muDrawsM1, dim = c(niter,P))

muDrawsM2 = fittedM0.9$draws(variables = 'mu')
niter = dim(muDrawsM2)[1]* dim(muDrawsM2)[2]
muDrawsM2 = array(muDrawsM2, dim = c(niter,P))

muDrawsM3 = fittedM0.95$draws(variables = 'mu')
niter = dim(muDrawsM3)[1]* dim(muDrawsM3)[2]
muDrawsM3 = array(muDrawsM3, dim = c(niter,P))

muDrawsM4 = fittedM0.995$draws(variables = 'mu')
niter = dim(muDrawsM4)[1]* dim(muDrawsM4)[2]
muDrawsM4 = array(muDrawsM4, dim = c(niter,P))

muDrawsM5 = fittedM0.999999$draws(variables = 'mu')
niter = dim(muDrawsM5)[1]* dim(muDrawsM5)[2]
muDrawsM5 = array(muDrawsM5, dim = c(niter,P))


PlotMu <- function(P, muDrawsM1, muDrawsM2, muDrawsM3, muDrawsM4, muDrawsM5){
  muDensM1 = vector('list', P)
  muDensM2 = vector('list', P)
  muDensM3 = vector('list', P)
  muDensM4 = vector('list', P)
  muDensM5 = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    muDensM1[[j]] = density(muDrawsM1[,j], from = -21, to = -6)
    muDensM2[[j]] = density(muDrawsM2[,j], from = -21, to = -6)
    muDensM3[[j]] = density(muDrawsM3[,j], from = -21, to = -6)
    muDensM4[[j]] = density(muDrawsM4[,j], from = -21, to = -6)
    muDensM5[[j]] = density(muDrawsM5[,j], from = -21, to = -6)
    
    
    yUpperLim = max(c(muDensM1[[j]]$y,muDensM2[[j]]$y,muDensM3[[j]]$y, muDensM4[[j]]$y, muDensM5[[j]]$y))  
    yLowerLim = min(c(muDensM1[[j]]$y,muDensM2[[j]]$y,muDensM3[[j]]$y, muDensM4[[j]]$y, muDensM5[[j]]$y))
    
    xUpperLim = max(c(muDensM1[[j]]$x,muDensM2[[j]]$x,muDensM3[[j]]$x, muDensM4[[j]]$x, muDensM5[[j]]$x))  
    xLowerLim = min(c(muDensM1[[j]]$x,muDensM2[[j]]$x,muDensM3[[j]]$x, muDensM4[[j]]$x, muDensM5[[j]]$x))
    
    
    plot(muDensM1[[j]]$x, muDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(mu[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim = c(xLowerLim, xUpperLim))
    
    lines(muDensM2[[j]]$x, muDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
    
    lines(muDensM3[[j]]$x, muDensM3[[j]]$y, type = "l", col = prettycol[3], lwd = 2.5, lty=1)
    
    lines(muDensM4[[j]]$x, muDensM4[[j]]$y, type = "l", col = prettycol[4], lwd = 2.5, lty=1)
    
    lines(muDensM5[[j]]$x, muDensM5[[j]]$y, type = "l", col = prettycol[5], lwd = 2.5, lty=1)
    
    #legend(x='topleft', legend = c('M0.8 Posterior', 'M0.9 Posterior', 'M0.95 Posterior', 'M0.995 Posterior', 'M0.999999 Posterior'), 
           #lty = c(1,1,1,1,1), lwd = c(2.5,2.5,2.5,2.5,2.5), col=c(prettycol[1], prettycol[2], prettycol[3], prettycol[4], prettycol[5]),
           #pt.cex = 3, cex = 0.60, seg.len=4)
  }
}

cairo_pdf("muSim_plot.pdf", width = 5, height = 6)
PlotMu(P=P, muDrawsM1=muDrawsM1, muDrawsM2=muDrawsM2, muDrawsM3=muDrawsM3, muDrawsM4=muDrawsM4, muDrawsM5=muDrawsM5)
dev.off()

###phi
phiDrawsM1 = fittedM0.8$draws(variables = 'phi')
niter = dim(phiDrawsM1)[1] * dim(phiDrawsM1)[2]
phiDrawsM1 = array(phiDrawsM1, dim = c(niter,P))

phiDrawsM2 = fittedM0.9$draws(variables = 'phi')
niter = dim(phiDrawsM2)[1]* dim(phiDrawsM2)[2]
phiDrawsM2 = array(phiDrawsM2, dim = c(niter,P))

phiDrawsM3 = fittedM0.95$draws(variables = 'phi')
niter = dim(phiDrawsM3)[1]* dim(phiDrawsM3)[2]
phiDrawsM3 = array(phiDrawsM3, dim = c(niter,P))

phiDrawsM4 = fittedM0.995$draws(variables = 'phi')
niter = dim(phiDrawsM4)[1]* dim(phiDrawsM4)[2]
phiDrawsM4 = array(phiDrawsM4, dim = c(niter,P))

phiDrawsM5 = fittedM0.999999$draws(variables = 'phi')
niter = dim(phiDrawsM5)[1]* dim(phiDrawsM5)[2]
phiDrawsM5 = array(phiDrawsM5, dim = c(niter,P))


Plotphi <- function(P, phiDrawsM1, phiDrawsM2, phiDrawsM3, phiDrawsM4, phiDrawsM5){
  phiDensM1 = vector('list', P)
  phiDensM2 = vector('list', P)
  phiDensM3 = vector('list', P)
  phiDensM4 = vector('list', P)
  phiDensM5 = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    phiDensM1[[j]] = density(phiDrawsM1[,j], from = -0.5, to = 1)
    phiDensM2[[j]] = density(phiDrawsM2[,j], from = -0.5, to = 1)
    phiDensM3[[j]] = density(phiDrawsM3[,j], from = -0.5, to = 1)
    phiDensM4[[j]] = density(phiDrawsM4[,j], from = -0.5, to = 1)
    phiDensM5[[j]] = density(phiDrawsM5[,j], from = -0.5, to = 1)
    
    
    yUpperLim = max(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y,phiDensM3[[j]]$y, phiDensM4[[j]]$y, phiDensM5[[j]]$y))  
    yLowerLim = min(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y,phiDensM3[[j]]$y, phiDensM4[[j]]$y, phiDensM5[[j]]$y))
    
    xUpperLim = max(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x,phiDensM3[[j]]$x, phiDensM4[[j]]$x, phiDensM5[[j]]$x))  
    xLowerLim = min(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x,phiDensM3[[j]]$x, phiDensM4[[j]]$x, phiDensM5[[j]]$x))
    
    
    plot(phiDensM1[[j]]$x, phiDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(phi[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim = c(xLowerLim, xUpperLim))
    
    lines(phiDensM2[[j]]$x, phiDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
    
    lines(phiDensM3[[j]]$x, phiDensM3[[j]]$y, type = "l", col = prettycol[3], lwd = 2.5, lty=1)
    
    lines(phiDensM4[[j]]$x, phiDensM4[[j]]$y, type = "l", col = prettycol[4], lwd = 2.5, lty=1)
    
    lines(phiDensM5[[j]]$x, phiDensM5[[j]]$y, type = "l", col = prettycol[5], lwd = 2.5, lty=1)
    
    #legend(x='topleft', legend = c('M0.8 Posterior', 'M0.9 Posterior', 'M0.95 Posterior', 'M0.995 Posterior', 'M0.999999 Posterior'), 
           #lty = c(1,1,1,1,1), lwd = c(2.5,2.5,2.5,2.5,2.5), col=c(prettycol[1], prettycol[2], prettycol[3], prettycol[4], prettycol[5]),
           #pt.cex = 3, cex = 0.60, seg.len=4)
  }
}

cairo_pdf("phiSim_plot.pdf", width = 5, height = 6)
Plotphi(P=P, phiDrawsM1=phiDrawsM1, phiDrawsM2=phiDrawsM2, phiDrawsM3=phiDrawsM3, phiDrawsM4=phiDrawsM4, phiDrawsM5=phiDrawsM5)
dev.off()

#Sigma
sigmaDrawsM1 = fittedM0.8$draws(variables = 'sigmaEps')
niter = dim(sigmaDrawsM1)[1] * dim(sigmaDrawsM1)[2]
sigmaDrawsM1 = as.vector(array(sigmaDrawsM1, dim = c(niter,1)))

sigmaDrawsM2 = fittedM0.9$draws(variables = 'sigmaEps')
niter = dim(sigmaDrawsM2)[1] * dim(sigmaDrawsM2)[2]
sigmaDrawsM2 = as.vector(array(sigmaDrawsM2, dim = c(niter,1)))

sigmaDrawsM3 = fittedM0.95$draws(variables = 'sigmaEps')
niter = dim(sigmaDrawsM3)[1] * dim(sigmaDrawsM3)[2]
sigmaDrawsM3 = as.vector(array(sigmaDrawsM3, dim = c(niter,1)))

sigmaDrawsM4 = fittedM0.995$draws(variables = 'sigmaEps')
niter = dim(sigmaDrawsM4)[1] * dim(sigmaDrawsM4)[2]
sigmaDrawsM4 = as.vector(array(sigmaDrawsM4, dim = c(niter,1)))

sigmaDrawsM5 = fittedM0.999999$draws(variables = 'sigmaEps')
niter = dim(sigmaDrawsM5)[1] * dim(sigmaDrawsM5)[2]
sigmaDrawsM1 = as.vector(array(sigmaDrawsM5, dim = c(niter,1)))

PlotSigma <- function(P, sigmaDrawsM1, sigmaDrawsM2, sigmaDrawsM3, sigmaDrawsM4, sigmaDrawsM5){
  
  
  sigmaDensM1 = density(sigmaDrawsM1)
  sigmaDensM2 = density(sigmaDrawsM2)
  sigmaDensM3 = density(sigmaDrawsM3)
  sigmaDensM4 = density(sigmaDrawsM4)
  sigmaDensM5 = density(sigmaDrawsM5)
  
  yUpperLim = max(c(sigmaDensM1$y,sigmaDensM2$y,sigmaDensM3$y,sigmaDensM4$y,sigmaDensM5$y))  
  yLowerLim = min(c(sigmaDensM1$y,sigmaDensM2$y,sigmaDensM3$y,sigmaDensM4$y,sigmaDensM5$y))
  
  xUpperLim = max(c(sigmaDensM1$x,sigmaDensM2$x,sigmaDensM3$x,sigmaDensM4$x,sigmaDensM5$x))  
  xLowerLim = min(c(sigmaDensM1$x,sigmaDensM2$x,sigmaDensM3$x,sigmaDensM4$x,sigmaDensM5$x))
  
  plot(sigmaDensM1$x, sigmaDensM1$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(sigma[epsilon]), 
       ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim, xUpperLim))
  
  lines(sigmaDensM2$x, sigmaDensM2$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
  
  lines(sigmaDensM3$x, sigmaDensM3$y, type = "l", col = prettycol[3], lwd = 2.5, lty=1)
  lines(sigmaDensM4$x, sigmaDensM4$y, type = "l", col = prettycol[4], lwd = 2.5, lty=1)
  lines(sigmaDensM5$x, sigmaDensM5$y, type = "l", col = prettycol[5], lwd = 2.5, lty=1)
  
  #legend(x='topleft', legend = c('M0.8 Posterior', 'M0.9 Posterior', 'M0.95 Posterior', 'M0.995 Posterior', 'M0.999999 Posterior'), 
         #lty = c(1,1,1,1,1), lwd = c(2.5,2.5,2.5,2.5,2.5), col=c(prettycol[1], prettycol[2], prettycol[3], prettycol[4], prettycol[5]),
         #pt.cex = 3, cex = 0.60, seg.len=4)
}

cairo_pdf("sigmaSim_plot.pdf", width = 6, height = 6)
PlotSigma(P=P, sigmaDrawsM1=sigmaDrawsM1, sigmaDrawsM2=sigmaDrawsM2, sigmaDrawsM3=sigmaDrawsM3, sigmaDrawsM4=sigmaDrawsM4, sigmaDrawsM5=sigmaDrawsM5)
dev.off()

#YPred

yPredM1 = fittedM0.8$draws(variables = 'yPred')
dimsYPred = dim(yPredM1)
nIter = dimsYPred[1]*dimsYPred[2] 
yPredM1 = array(yPredM1, dim = c(nIter, T))

yPredM2 = fittedM0.9$draws(variables = 'yPred')
dimsYPred = dim(yPredM2)
nIter = dimsYPred[1]*dimsYPred[2] 
yPredM2 = array(yPredM2, dim = c(nIter, T))

yPredM3 = fittedM0.95$draws(variables = 'yPred')
dimsYPred = dim(yPredM3)
nIter = dimsYPred[1]*dimsYPred[2] 
yPredM3 = array(yPredM3, dim = c(nIter, T))

yPredM4 = fittedM0.995$draws(variables = 'yPred')
dimsYPred = dim(yPredM4)
nIter = dimsYPred[1]*dimsYPred[2] 
yPredM4 = array(yPredM4, dim = c(nIter, T))

yPredM5 = fittedM0.999999$draws(variables = 'yPred')
dimsYPred = dim(yPredM5)
nIter = dimsYPred[1]*dimsYPred[2] 
yPredM5 = array(yPredM5, dim = c(nIter, T))

densYM1 = density(yPredM1)
densYM2 = density(yPredM2)
densYM3 = density(yPredM3)
densYM4 = density(yPredM4)
densYM5 = density(yPredM5)

#cairo_pdf("yPredSim_plot.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYM1$x, densYM1$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYM2$x, densYM2$y, lty = 1, lwd = 2.5, col = prettycol[2])
lines(densYM3$x, densYM3$y, lty = 1, lwd = 2.5, col = prettycol[3])
lines(densYM4$x, densYM4$y, lty = 1, lwd = 2.5, col = prettycol[4])
lines(densYM5$x, densYM5$y, lty = 1, lwd = 2.5, col = prettycol[5])

legend(x = 'topright', legend = ('True y'),
       lty = (NA), lwd = (NA), col = c(NA),
       pch = c(22), pt.bg = c(prettycol[8]),
       pt.cex = 2, cex = 0.80)

#Apparenttly the divergencies doesn't have much impact, maybe due to the high number of iterations?!
#dev.off()


