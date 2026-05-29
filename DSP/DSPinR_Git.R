
library('HDInterval')
#library('dsp')
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

###Comparing differents runs of the model - original DSP (CC) -> nsave = 4000, nburn = 1000####
###DSP_FIT - Orignal
#for (j in 1:3){
  #set.seed(j)
  
  #modelSpec <- dsp::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'const')
  
  #fit <- dsp::dsp_fit(y=y, model_spec = modelSpec, nsave = 4000, nburn = 1000)
  
  #saveRDS(fit, sprintf("DSPModel%d.rds", j))
  
#}

#We use dataset with different seeds, to try to understand if the model was converging

##Seems to have poor mixing with regard of parameter = 1!
fitSavedM1 = readRDS('DSPModel1.rds')
fitSavedM2 = readRDS('DSPModel2.rds')
fitSavedM3 = readRDS('DSPModel3.rds')

#Betas
betaDrawsM1 = fitSavedM1$mcmc_output$beta

betaDrawsM2 = fitSavedM2$mcmc_output$beta

betaDrawsM3 = fitSavedM3$mcmc_output$beta



x <- 1:nrow(betas)

PlotBetas <- function(betas, betaDrawsM1, betaDrawsM2,betaDrawsM3, x , j){
  
  medianBetaM1 <- apply(betaDrawsM1, c(2, 3), median)
  hdiM1 = apply(betaDrawsM1, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM2 <- apply(betaDrawsM2, c(2, 3), median)
  hdiM2 = apply(betaDrawsM2, c(2,3), hdi, credMass = 0.95)
  
  medianBetaM3 <- apply(betaDrawsM3, c(2, 3), median)
  hdiM3 = apply(betaDrawsM3, c(2,3), hdi, credMass = 0.95)
  
  lowerlim = min(c(betas[,j], hdiM1[1,,j], hdiM2[1,,j], hdiM3[1,,j]))
  upperlim = max(c(betas[,j], hdiM1[2,,j], hdiM2[2,,j], hdiM3[2,,j]))
  
  plot(x, betas[,j], type = "l", col = "black", lwd = 2.5, main = bquote(beta[.(j)]), 
       ylab = "", xlab = "time", ylim = c(lowerlim, upperlim))
  
  polygon(c(x, rev(x)), c(hdiM1[1,,j], rev(hdiM1[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiM2[1,,j], rev(hdiM2[2,,j])),
          col = NA, border = prettycol[6])
  
  polygon(c(x, rev(x)), c(hdiM3[1,,j], rev(hdiM3[2,,j])),
          col = NA, border = prettycol[7])
  
  lines(medianBetaM1[ ,j], col = prettycol[1], lwd = 2)
  
  lines(medianBetaM2[ ,j], col = prettycol[2], lwd = 2)
  
  lines(medianBetaM3[ ,j], col = prettycol[3], lwd = 2)
  
  legend(x = 'bottomright', legend = c('True Median', 'M1 Median', 'M2 Median', 'M3 Median',
                                       '95% HPD M1', '95% HPD M2', '95% HPD M3'), 
         col = c('black', prettycol[1], prettycol[2], prettycol[3], NA, prettycol[6], prettycol[7]), 
         lwd = c(2.5, 2, 2, 2, NA, 1, 1), 
         lty = c(1, 1, 1, 1, NA, 1, 1),
         pch = c(NA, NA, NA, NA, 22, NA, NA),
         pt.bg = c(NA, NA, NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30),
                   NA, NA),
         pt.cex = 2, cex = 0.60, seg.len=4
  )
  
}

#Betas1
PlotBetas(betas = betas, betaDrawsM1 = betaDrawsM1, betaDrawsM2 = betaDrawsM2, betaDrawsM3 = betaDrawsM3,
          x = x, j = 1)

#Betas2
PlotBetas(betas = betas, betaDrawsM1 = betaDrawsM1, betaDrawsM2 = betaDrawsM2, betaDrawsM3 = betaDrawsM3,
          x = x, j = 2)

#Betas3
PlotBetas(betas = betas, betaDrawsM1 = betaDrawsM1, betaDrawsM2 = betaDrawsM2, betaDrawsM3 = betaDrawsM3,
          x = x, j = 3)


#Mu
muDrawsM1 = fitSavedM1$mcmc_output$dhs_mean
muDrawsM2 = fitSavedM2$mcmc_output$dhs_mean
muDrawsM3 = fitSavedM3$mcmc_output$dhs_mean


PlotMu <- function(P, muDrawsM1, muDrawsM2, muDrawsM3){
  muDensM1 = vector('list', P)
  muDensM2 = vector('list', P)
  muDensM3 = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    muDensM1[[j]] = density(muDrawsM1[,j])
    muDensM2[[j]] = density(muDrawsM2[,j])
    muDensM3[[j]] = density(muDrawsM3[,j])
    
    yUpperLim = max(c(muDensM1[[j]]$y,muDensM2[[j]]$y,muDensM3[[j]]$y))  
    yLowerLim = min(c(muDensM1[[j]]$y,muDensM2[[j]]$y,muDensM3[[j]]$y))
    
    xUpperLim = max(c(muDensM1[[j]]$x,muDensM2[[j]]$x,muDensM3[[j]]$x))  
    xLowerLim = min(c(muDensM1[[j]]$x,muDensM2[[j]]$x,muDensM3[[j]]$x))
    
    plot(muDensM1[[j]]$x, muDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(mu[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim = c(xLowerLim, xUpperLim))
    
    lines(muDensM2[[j]]$x, muDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
    
    lines(muDensM3[[j]]$x, muDensM3[[j]]$y, type = "l", col = prettycol[3], lwd = 2.5, lty=1)
    
    legend(x='topleft', legend = c('M1 Posterior', 'M2 Posterior', 'M3 Posterior'), lty = c(1,1,1), lwd = c(2.5,2.5,2.5), 
           col=c(prettycol[1], prettycol[2], prettycol[3]), pt.cex = 3, cex = 0.75, seg.len=4)
  }
}

PlotMu(P=P, muDrawsM1=muDrawsM1, muDrawsM2=muDrawsM2, muDrawsM3=muDrawsM3)


#Phi
phiDrawsM1 = fitSavedM1$mcmc_output$dhs_phi
phiDrawsM2 = fitSavedM2$mcmc_output$dhs_phi
phiDrawsM3 = fitSavedM3$mcmc_output$dhs_phi

PlotPhi <- function(P, phiDrawsM1, phiDrawsM2,phiDrawsM3){
  phiDensM1 = vector('list', P)
  phiDensM2 = vector('list', P)
  phiDensM3 = vector('list', P)
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    phiDensM1[[j]] = density(phiDrawsM1[,j])
    phiDensM2[[j]] = density(phiDrawsM2[,j])
    phiDensM3[[j]] = density(phiDrawsM3[,j])
    
    yUpperLim = max(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y,phiDensM3[[j]]$y))  
    yLowerLim = min(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y,phiDensM3[[j]]$y))
    
    xUpperLim = max(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x,phiDensM3[[j]]$x))  
    xLowerLim = min(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x,phiDensM3[[j]]$x))
    
    plot(phiDensM1[[j]]$x, phiDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(phi[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim,xUpperLim))
    
    lines(phiDensM2[[j]]$x, phiDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5)
    
    lines(phiDensM3[[j]]$x, phiDensM3[[j]]$y, type = "l", col = prettycol[3], lwd = 2.5)
    
    legend(x='topleft', legend = c('M1 Posterior', 'M2 Posterior', 'M3 Posterior'), lty = c(1,1,1), lwd = c(2.5,2.5,2.5), 
           col=c(prettycol[1], prettycol[2], prettycol[3]), pt.cex = 3, cex = 0.75, seg.len=4)
  }
}

PlotPhi(P=P, phiDrawsM1=phiDrawsM1, phiDrawsM2=phiDrawsM2, phiDrawsM3=phiDrawsM3)


#SD
varianceSigmaDrawsM1 = fitSavedM1$mcmc_output$obs_sigma_t2 
sigmaEpsDrawsM1 = sqrt(varianceSigmaDrawsM1[,1])

varianceSigmaDrawsM2 = fitSavedM2$mcmc_output$obs_sigma_t2 
sigmaEpsDrawsM2 = sqrt(varianceSigmaDrawsM2[,1])

varianceSigmaDrawsM3 = fitSavedM3$mcmc_output$obs_sigma_t2 
sigmaEpsDrawsM3 = sqrt(varianceSigmaDrawsM3[,1])

PlotSigma <- function(P, sigmaEpsDrawsM1, sigmaEpsDrawsM2, sigmaEpsDrawsM3){
  
  sigmaDensM1 = density(sigmaEpsDrawsM1)
  sigmaDensM2 = density(sigmaEpsDrawsM2)
  sigmaDensM3 = density(sigmaEpsDrawsM3)
  
  yUpperLim = max(c(sigmaDensM1$y,sigmaDensM2$y,sigmaDensM3$y))  
  yLowerLim = min(c(sigmaDensM1$y,sigmaDensM2$y,sigmaDensM3$y))
  
  xUpperLim = max(c(sigmaDensM1$x,sigmaDensM2$x,sigmaDensM3$x))  
  xLowerLim = min(c(sigmaDensM1$x,sigmaDensM2$x,sigmaDensM3$x))
  
  plot(sigmaDensM1$x, sigmaDensM1$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(sigma[epsilon]), 
       ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim, xUpperLim))
  
  lines(sigmaDensM2$x, sigmaDensM2$y, type = "l", col = prettycol[2], lwd = 2.5)
  
  lines(sigmaDensM3$x, sigmaDensM3$y, type = "l", col = prettycol[3], lwd = 2.5)
  
  legend(x='topleft', legend = c('M1 Posterior', 'M2 Posterior', 'M3 Posterior'), lty = c(1,1,1), lwd = c(2.5,2.5,2.5), 
         col=c(prettycol[1], prettycol[2], prettycol[3]), pt.cex = 3, cex = 0.75, seg.len=4)
}

PlotSigma(P=P, sigmaEpsDrawsM1=sigmaEpsDrawsM1, sigmaEpsDrawsM2=sigmaEpsDrawsM2, sigmaEpsDrawsM3=sigmaEpsDrawsM3)


##################Comparing diff DSP Package############


#OriginalDSP
#modelSpec <- dsp::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'const')
#modelOrigDSP <- dsp::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelOrigDSP, 'OrigDSPComp2.rds')

#MyVersionDSP
#modelSpec2 <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'const')
#modelMyVerDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec2, nsave = 12000, nburn = 1000)
#saveRDS(modelMyVerDSP, 'MyVerDSPComp.rds')

#Note DSP* is the perfected one!!!!!
modelOrig = readRDS('OrigDSPComp.rds')
modelMyVer = readRDS('MyVerDSPComp.rds')

#Betas
betaDrawsOrig = modelOrig$mcmc_output$beta

betaDrawsMy = modelMyVer$mcmc_output$beta

x <- 1:nrow(betas)

PlotBetas <- function(betas, betaDrawsOrig, betaDrawsMy, x , j){
  
  medianBetaMO <- apply(betaDrawsOrig, c(2, 3), median)
  hdiBetaMO = apply(betaDrawsOrig, c(2, 3), hdi, massCred = 0.95)
  
  medianBetaMy <- apply(betaDrawsMy, c(2, 3), median)
  hdiBetaMy = apply(betaDrawsMy, c(2, 3), hdi, massCred = 0.95)
  
  lowerlim = min(c(betas[,j], hdiBetaMO[1,,j], hdiBetaMy[1,,j]))
  upperlim = max(c(betas[,j], hdiBetaMO[2,,j], hdiBetaMy[2,,j]))
  
  plot(x, betas[,j], type = "l", col = "black", lwd = 2.5, main = bquote(beta[.(j)]), 
       ylab = "", xlab = "time", ylim = c(lowerlim, upperlim))
  
  polygon(c(x, rev(x)), c(hdiBetaMO[1,,j], rev(hdiBetaMO[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiBetaMy[1,,j], rev(hdiBetaMy[2,,j])),
          col = NA, border = prettycol[6])
  
  lines(medianBetaMO[ ,j], col = prettycol[1], lwd = 2)
  
  lines(medianBetaMy[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Parameter Evolution', 'Median DSP', 'Median DSP*',
                                       '95% HPD DSP', '95% HPD DSP*'), 
         col = c('black', prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2.5, 2, 2, 1, 1), 
         lty = c(1, 1, 1, 1, 1),
         pch = c(NA, NA, NA, 22, NA),
         pt.bg = c(NA, NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2, cex = 0.80, seg.len=4
  )
  
}

#Betas1
cairo_pdf("Beta1Sim_plotDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, betaDrawsMy = betaDrawsMy, betaDrawsOrig = betaDrawsOrig, x = x, j = 1)
dev.off()

#Betas2
cairo_pdf("Beta2Sim_plotDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, betaDrawsMy = betaDrawsMy, betaDrawsOrig = betaDrawsOrig, x = x, j = 2)
dev.off()

#Betas3
cairo_pdf("Beta3Sim_plotDSP.pdf", width = 10, height = 7)
PlotBetas(betas = betas, betaDrawsMy = betaDrawsMy, betaDrawsOrig = betaDrawsOrig, x = x, j = 3)
dev.off()

#width C.I.

medianBetaMO <- apply(betaDrawsOrig, c(2, 3), median)
hdiBetaMO = apply(betaDrawsOrig, c(2, 3), hdi, massCred = 0.95)

medianBetaMy <- apply(betaDrawsMy, c(2, 3), median)
hdiBetaMy = apply(betaDrawsMy, c(2, 3), hdi, massCred = 0.95)

widthMO = hdiBetaMO[2, ,] - hdiBetaMO[1, ,]
meanWidthB1 = mean(widthMO[ ,1])
meanWidthB2 = mean(widthMO[ ,2])
meanWidthB3 = mean(widthMO[ ,3])

widthMy = hdiBetaMy[2, ,] - hdiBetaMy[1, ,]

meanWidthB1My = mean(widthMy[ ,1])
meanWidthB2My = mean(widthMy[ ,2])
meanWidthB3My = mean(widthMy[ ,3])


#Mu
muDrawsMO = modelOrig$mcmc_output$dhs_mean
muDrawsMy = modelMyVer$mcmc_output$dhs_mean


PlotMu <- function(P, muDrawsM1, muDrawsM2){
  muDensM1 = vector('list', P)
  muDensM2 = vector('list', P)
  
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    muDensM1[[j]] = density(muDrawsM1[,j])
    muDensM2[[j]] = density(muDrawsM2[,j])
    
    yUpperLim = max(c(muDensM1[[j]]$y,muDensM2[[j]]$y))  
    yLowerLim = min(c(muDensM1[[j]]$y,muDensM2[[j]]$y))
    
    xUpperLim = max(c(muDensM1[[j]]$x,muDensM2[[j]]$x))  
    xLowerLim = min(c(muDensM1[[j]]$x,muDensM2[[j]]$x))
    
    plot(muDensM1[[j]]$x, muDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(mu[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim = c(xLowerLim, xUpperLim))
    
    lines(muDensM2[[j]]$x, muDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5, lty=1)
    
    
    legend(x='topleft', legend = c('DSP Posterior', 'DSP* Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
           col=c(prettycol[1], prettycol[2]), pt.cex = 3, cex = 0.75, seg.len=4)
  }
}
cairo_pdf("MuSim_plotDSP.pdf", width = 7, height = 7)
PlotMu(P=P, muDrawsM1=muDrawsMO, muDrawsM2=muDrawsMy)
dev.off()

#Phi
phiDrawsMO = modelOrig$mcmc_output$dhs_phi
phiDrawsMy = modelMyVer$mcmc_output$dhs_phi


PlotPhi <- function(P, phiDrawsM1, phiDrawsM2){
  phiDensM1 = vector('list', P)
  phiDensM2 = vector('list', P)
  
  
  par(mfrow = c(P,1))
  for (j in 1:P){
    phiDensM1[[j]] = density(phiDrawsM1[,j])
    phiDensM2[[j]] = density(phiDrawsM2[,j])
    
    
    yUpperLim = max(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y))  
    yLowerLim = min(c(phiDensM1[[j]]$y,phiDensM2[[j]]$y))
    
    xUpperLim = max(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x))  
    xLowerLim = min(c(phiDensM1[[j]]$x,phiDensM2[[j]]$x))
    
    plot(phiDensM1[[j]]$x, phiDensM1[[j]]$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(phi[.(j)]), 
         ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim,xUpperLim))
    
    lines(phiDensM2[[j]]$x, phiDensM2[[j]]$y, type = "l", col = prettycol[2], lwd = 2.5)
    
    
    
    legend(x='topleft', legend = c('DSP Posterior', 'DSP* Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
           col=c(prettycol[1], prettycol[2]), pt.cex = 3, cex = 0.75, seg.len=4)
  }
}

cairo_pdf("PhiSim_plotDSP.pdf", width = 7, height = 7)
PlotPhi(P=P, phiDrawsM1=phiDrawsMO, phiDrawsM2=phiDrawsMy)
dev.off()

#SD
varianceSigmaDrawsM1 = modelOrig$mcmc_output$obs_sigma_t2 
sigmaEpsDrawsM1 = sqrt(varianceSigmaDrawsM1[,1])

varianceSigmaDrawsM2 = modelMyVer$mcmc_output$obs_sigma_t2 
sigmaEpsDrawsM2 = sqrt(varianceSigmaDrawsM2[,1])


PlotSigma <- function(P, sigmaEpsDrawsM1, sigmaEpsDrawsM2){
  
  sigmaDensM1 = density(sigmaEpsDrawsM1)
  sigmaDensM2 = density(sigmaEpsDrawsM2)
  
  
  yUpperLim = max(c(sigmaDensM1$y,sigmaDensM2$y))  
  yLowerLim = min(c(sigmaDensM1$y,sigmaDensM2$y))
  
  xUpperLim = max(c(sigmaDensM1$x,sigmaDensM2$x))  
  xLowerLim = min(c(sigmaDensM1$x,sigmaDensM2$x))
  
  plot(sigmaDensM1$x, sigmaDensM1$y, type = "l", col = prettycol[1], lwd = 2.5, main = bquote(sigma[epsilon]), 
       ylab = "", xlab = "", ylim=c(yLowerLim, yUpperLim), xlim=c(xLowerLim, xUpperLim))
  
  lines(sigmaDensM2$x, sigmaDensM2$y, type = "l", col = prettycol[2], lwd = 2.5)
  
  
  legend(x='topleft', legend = c('DSP Posterior', 'DSP* Posterior'), lty = c(1,1), lwd = c(2.5,2.5), 
         col=c(prettycol[1], prettycol[2]), pt.cex = 3, cex = 0.75, seg.len=4)
}

cairo_pdf("SigmaSim_plotDSP.pdf", width = 7, height = 7)
PlotSigma(P=P, sigmaEpsDrawsM1=sigmaEpsDrawsM1, sigmaEpsDrawsM2=sigmaEpsDrawsM2)
dev.off()


#YPred
yPredDspOrig = modelOrig$mcmc_output$yhat
yPredDspMy = modelMyVer$mcmc_output$yhat


densYDspOrig = density(yPredDspOrig)
densYDspMy = density(yPredDspMy)


cairo_pdf("yHatSim_plotDSP.pdf", width = 7, height = 6)

hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYDspOrig$x, densYDspOrig$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDspMy$x, densYDspMy$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - DSP', 'Pred. y - DSP*'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()




