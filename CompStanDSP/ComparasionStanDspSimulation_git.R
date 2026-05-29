setwd("C:/Users/gonca/Documents/Uni/4th semester/RFiles")

library('HDInterval')
library('cmdstanr')
library('dspPerfected')
set.seed(123)
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666",
              "#9E4F4F", "#2F4858", "#7A6C5D","#3A7D7D")

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


#####Compare Synthetic Data#####
#Note that you will need to generate all the models first; but the other files have the code
#for that!
stanVerM <- readRDS("AlbinoRstanDSP/code/stan/fitCmdstanr0.995w1chain.rds") 
dspVerM <- readRDS('DSPPackage/MyVerDSPComp.rds') 
stanModel0.8 = readRDS("AlbinoRstanDSP/code/stan/fitCmdstanr0.8w1chains.rds")

#################Compare Betas Posterior

betaDrawsStan = stanVerM$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = dspVerM$mcmc_output$beta



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])
meanWidthB3 = mean(widthStan[ ,3])

widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])
meanWidthB3Dsp = mean(widthDsp[ ,3])


x <- 1:nrow(betas)

PlotBetas <- function(betas, medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp){
  
  lowerlim = min(c(betas[,j], hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(betas[,j], hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, betas[,j], type = "l", col = "black", lwd = 2.5, main = bquote(beta[.(j)]), 
       ylab = "", xlab = "time", ylim = c(lowerlim, upperlim))
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Parameter Evolution', 'Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c('black', prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2.5, 2, 2, NA, 1), 
         lty = c(1, 1, 1, NA, 1),
         pch = c(NA, NA, NA, 22, NA),
         pt.bg = c(NA, NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.80, seg.len=4
  )
  
}

#Beta1
cairo_pdf("Beta1Sim_plotStanDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp)
dev.off()

#Beta2
cairo_pdf("Beta2Sim_plotStanDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp)
dev.off()

#Beta3
cairo_pdf("Beta3Sim_plotStanDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 3, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp)
dev.off()

#################Compare Mu Posterior
muDrawsStan = stanVerM$draws(variables = 'mu')
niter = dim(muDrawsStan)[1]
muDrawsStan = array(muDrawsStan, dim = c(niter,P))

muDrawsDsp = dspVerM$mcmc_output$dhs_mean

PlotMu <- function(P, muDrawsStan, muDrawsDsp){
  muDensStan = vector('list', P)
  muDensDsp = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    muDensStan[[j]] = density(muDrawsStan[,j], from = -20, to = -5)
    muDensDsp[[j]] = density(muDrawsDsp[,j], from = -20, to = -5)
    
    yUpperLim = max(c(muDensStan[[j]]$y,muDensDsp[[j]]$y))  
    yLowerLim = min(c(muDensStan[[j]]$y,muDensDsp[[j]]$y))
    
    xUpperLim = max(c(muDensStan[[j]]$x,muDensDsp[[j]]$x))  
    xLowerLim = min(c(muDensStan[[j]]$x,muDensDsp[[j]]$x))
    
    plot(muDensStan[[j]]$x, muDensStan[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(mu[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim = c(xLowerLim, xUpperLim))
    lines(muDensDsp[[j]]$x, muDensDsp[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5)
    
    legend(x='topleft', legend = c('Stan Posterior', 'DSP Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
           col=c(prettycol[1], prettycol[2]), pt.cex = 2, cex = 0.85, seg.len=4)
  }
}

cairo_pdf("MuSim_plotStanDSP.pdf", width = 7, height = 7)
PlotMu(P=P, muDrawsStan = muDrawsStan, muDrawsDsp = muDrawsDsp)
dev.off()


#################Compare Phi Posterior

phiDrawsStan = stanVerM$draws(variables = 'phi')
niter = dim(phiDrawsStan)[1]
phiDrawsStan = array(phiDrawsStan, dim = c(niter, P))

phiDrawsDsp = dspVerM$mcmc_output$dhs_phi

PlotPhi <- function(P, phiDrawsStan, phiDrawsDsp){
  phiDensStan = vector('list', P)
  phiDensDsp = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    phiDensStan[[j]] = density(phiDrawsStan[,j], from = -0.5, to = 1.5)
    phiDensDsp[[j]] = density(phiDrawsDsp[,j], from = -0.5, to = 1.5)
   
    yUpperLim = max(c(phiDensStan[[j]]$y,phiDensDsp[[j]]$y))  
    yLowerLim = min(c(phiDensStan[[j]]$y,phiDensDsp[[j]]$y))
    
    xUpperLim = max(c(phiDensStan[[j]]$x,phiDensDsp[[j]]$x))  
    xLowerLim = min(c(phiDensStan[[j]]$x,phiDensDsp[[j]]$x))
    
    plot(phiDensStan[[j]]$x, phiDensStan[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(phi[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim,xUpperLim))
    lines(phiDensDsp[[j]]$x, phiDensDsp[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5)
    
    legend(x='topleft', legend = c('Stan Posterior', 'DSP Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
           col=c(prettycol[1], prettycol[2]), pt.cex = 2, cex = 0.85, seg.len=4)
  }
}

cairo_pdf("PhiSim_plotStanDSP.pdf", width = 7, height = 7)
PlotPhi(P=P, phiDrawsStan = phiDrawsStan, phiDrawsDsp = phiDrawsDsp)
dev.off()


#################Compare Sigma Posterior (SD)

sigmaEpsDrawsStan = as.vector(stanVerM$draws(variables= 'sigmaEps'))

varianceSigmaDrawsDps = dspVerM$mcmc_output$obs_sigma_t2 
#Dim(variance...) = 12000*500, however the cols have the same numbers! So,
#we can use only the first col! They have 500 copies due to computation purposes ?!
sigmaEpsDrawsDps = sqrt(varianceSigmaDrawsDps[,1])

PlotSigma <- function(P, sigmaDrawsStan, sigmaDrawsDsp){

  
  sigmaDensStan = density(sigmaDrawsStan)
  sigmaDensDsp = density(sigmaDrawsDsp)
    
  yUpperLim = max(c(sigmaDensStan$y,sigmaDensDsp$y))  
  yLowerLim = min(c(sigmaDensStan$y,sigmaDensDsp$y))
  
  xUpperLim = max(c(sigmaDensStan$x,sigmaDensDsp$x))  
  xLowerLim = min(c(sigmaDensStan$x,sigmaDensDsp$x))
    
  plot(sigmaDensStan$x, sigmaDensStan$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(sigma[epsilon]), 
      ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim, xUpperLim))
  lines(sigmaDensDsp$x, sigmaDensDsp$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
    
  legend(x='topright', legend = c('Stan Posterior', 'DSP Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
           col=c(prettycol[1], prettycol[2]), pt.cex = 3, cex = 0.85, seg.len=4)
}

cairo_pdf("SigmaSim_plotStanDSP.pdf", width = 7, height = 7)
PlotSigma(P=P, sigmaDrawsStan = sigmaEpsDrawsStan, sigmaDrawsDsp = sigmaEpsDrawsDps)
dev.off()

################ y_pred
yPredStan = stanVerM$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = dspVerM$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("yHatSim_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()

###############ESS
library('coda')

##REPORT THE TOTAL TIMES IN THE THESIS! 
#Stan TotalTime
timeInSec = stanVerM$time()
totalTimeInSec = timeInSec$total

totalTimeInMinStan = totalTimeInSec / 60

timeInSecM0.8 = stanModel0.8$time()
totalTimeInSecM0.8 = timeInSecM0.8$total

totalTimeInMinStanM0.8 = totalTimeInSecM0.8 / 60

#DSP Total Time -> 00:03:55
totalTimeInMinDsp = 3 + (55/60)

###Betas - Coda doesn't accept: niter x T x P -> We need to reshape it!
#before we had 3 parameters: Beta1 with [12000 * 500], Beta2 with [12000 * 500] 
niter = dim(betaDrawsDsp)[1]
T = dim(betaDrawsDsp)[2]
P = dim(betaDrawsDsp)[3]

reshapeBetasDrawsDsp = array(betaDrawsDsp, dim = c(niter, T * P)) #niter x (T *P) [Now we have, niter x parameter_chain; each col has all values of Beta_1,1;...]
essBetasDrawsDsp = effectiveSize(reshapeBetasDrawsDsp)

essBetasDrawsDspPerMin = essBetasDrawsDsp/totalTimeInMinDsp

niter2 = dim(betaDrawsStan)[1]
T2 = dim(betaDrawsStan)[2]
P2 = dim(betaDrawsStan)[3]

reshapeBetasDrawsStan= array(betaDrawsStan, dim = c(niter2, T2 * P2)) 
essBetasDrawsStan = effectiveSize(reshapeBetasDrawsStan)

essBetasDrawsStanPerMin = essBetasDrawsStan/totalTimeInMinStan


betaDrawsStanM0.8 = stanModel0.8$draws(variables = 'betas')
niter2 = dim(betaDrawsStanM0.8)[1]
T2 = dim(betaDrawsStanM0.8)[2]
P2 = dim(betaDrawsStanM0.8)[3]

reshapeBetasDrawsStanM0.8= array(betaDrawsStanM0.8, dim = c(niter2, T2 * P2)) 
essBetasDrawsStanM0.8 = effectiveSize(reshapeBetasDrawsStanM0.8)

essBetasDrawsStanPerMinM0.8 = essBetasDrawsStanM0.8/totalTimeInMinStanM0.8

#Hist ESS/Min
xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsStanPerMinM0.8))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsStanPerMinM0.8))

cairo_pdf("histESSsim_plotStan2xDSP.pdf", width = 11, height = 8)
par(mfrow = c(3,1),
    cex.main = 2.0,   
    cex.lab  = 1.7,   
    cex.axis = 1.4) 
hist(essBetasDrawsStanPerMinM0.8, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[1], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute for" ~ beta[j*","*t]))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = '' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute for" ~ beta[j*","*t]))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute for" ~ beta[j*","*t]))
dev.off()




#ESS x Time
matrixEssBetasDSP = array(essBetasDrawsDsp, dim = c(T,P))

par(mfrow = c(3,1), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,1], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(1)]), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,2], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(2)]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,3], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(3)]), lwd = 2, lty = 1, col = prettycol[3], type = 'l')
mtext("DSP Version", outer = TRUE, cex = 1.5, font = 2)


matrixEssBetasStan= array(essBetasDrawsStan, dim = c(T,P))

par(mfrow = c(3,1), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,1], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(1)]), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')

plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,2], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(2)]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,3], xlab = 'Time', ylab = 'ESS', main = bquote(beta[t*","*.(3)]), lwd = 2, lty = 1, col = prettycol[3], type = 'l')
mtext("Stan Version", outer = TRUE, cex = 1.5, font = 2)



###MU
essMuDsp = effectiveSize(muDrawsDsp)
essMuDsp

essMuDspPerMin = essMuDsp/totalTimeInMinDsp
essMuDspPerMin

essMuStan = effectiveSize(muDrawsStan)
essMuStan

essMuStanPerMin = essMuStan/totalTimeInMinStan
essMuStanPerMin

###Phi
essPhiDsp = effectiveSize(phiDrawsDsp)
essPhiDsp

essPhiDspPerMin = essPhiDsp/totalTimeInMinDsp
essPhiDspPerMin

essPhiStan = effectiveSize(phiDrawsStan)
essPhiStan

essPhiStanPerMin = essPhiStan/totalTimeInMinStan
essPhiStanPerMin

###Sigma
essSigDsp = effectiveSize(sigmaEpsDrawsDps)
essSigDsp

essSigDspPerMin = essSigDsp/totalTimeInMinDsp
essSigDspPerMin

essSigStan = effectiveSize(sigmaEpsDrawsStan)
essSigStan

essSigStanPerMin = essSigStan/totalTimeInMinStan
essSigStanPerMin



