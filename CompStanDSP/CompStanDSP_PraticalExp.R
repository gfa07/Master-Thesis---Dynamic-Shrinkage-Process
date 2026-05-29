setwd("C:/Users/gonca/Documents/Uni/4th semester/CAPM DAta")


prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666",
              "#9E4F4F", "#2F4858", "#7A6C5D","#3A7D7D")
library('HDInterval')

###############CAPM-DATA#################

load("dataPraticalExp.Rda")

#############y-IBM###########
y = data$IBM - data$RKFREE
x = data$MARKET - data$RKFREE
X = cbind(1,x)


#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "IBMResponseModDSP,4.47min.rds")

modelMyVer = readRDS("IBMResponseModDSP,4.47min.rds")

modelStan = readRDS("fit_CAPMDataNonCentSV.rds")


T = nrow(X)
P = ncol(X) 

betaDrawsStan= modelStan$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVer$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])


widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])


x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta))

#Betas
PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}

cairo_pdf("alpha_IBM_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("beta_IBM_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


##SD
PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStan$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVer$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

cairo_pdf("Sd_SV_IBM_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

#time - comparison
timeInSec = modelStan$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 4 + (47/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

cairo_pdf("IBM_hist_ESS_Prac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
dev.off()

#Ypred
yPredStan = modelStan$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVer$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("IBM_y_Hat_Prac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()

###########y-DELTA###########
rm(list = ls()[!ls() %in% c("prettycol", "data")])

y = data$DELTA - data$RKFREE
x = data$MARKET - data$RKFREE
X = cbind(1,x)

#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "DeltaResponseModDSP,3.27min.rds")


modelMyVer = readRDS("DeltaResponseModDSP,3.27min.rds")
modelStan = readRDS("fit_DeltaDataNonCentSV.rds")

T = nrow(X)
P = ncol(X) 

betaDrawsStan= modelStan$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVer$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])


widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])


x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta))

#Betas
PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}
cairo_pdf("alpha_Delta_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("beta_Delta_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStan$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVer$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

cairo_pdf("Sd_SV_Delta_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

#time - comparison
timeInSec = modelStan$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 3 + (27/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

cairo_pdf("Delta_hist_ESS_Prac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
dev.off()

matrixEssBetasStan= array(essBetasDrawsStan, dim = c(T,P))

cairo_pdf("Delta_N_eff_Time_Stan.pdf", width = 10, height = 8)
par(mfrow = c(2,1), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,1], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(alpha[t]), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
#abline(h = 10000, col = 'red')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,2], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(beta[t]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
#abline(h = 3000, col ='red')
mtext("Stan Version", outer = TRUE, cex = 1.5, font = 2)
#Shows the blocks on the hist, because for alpha the values are aroun 10k abd for beta around 3k
dev.off()

median(matrixEssBetasStan[ ,1])

#Ypred
yPredStan = modelStan$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVer$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("Delta_y_Hat_Prac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()


#########y-MOBIL#########
rm(list = ls()[!ls() %in% c("prettycol", "data")])

y = data$MOBIL - data$RKFREE
x = data$MARKET - data$RKFREE
X = cbind(1,x)

#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "MobilResponseModDSP,3.08min.rds")

modelMyVer = readRDS("MobilResponseModDSP,3.08min.rds")


modelStan = readRDS("fit_MobilDataNonCentSV.rds") #note the time it took, could be a good argument!
#discard the results from tetralith if it takes the same time!

T = nrow(X)
P = ncol(X) 

betaDrawsStan = modelStan$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVer$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])


widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])


x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta))

#Betas
PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}

cairo_pdf("alpha_Mobil_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("beta_Mobil_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStan$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVer$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

cairo_pdf("Sd_SV_Mobil_Prac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

#time - comparison
timeInSec = modelStan$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 3 + (08/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

cairo_pdf("Mobil_hist_ESS_Prac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
dev.off()

#Ypred
yPredStan = modelStan$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVer$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("Mobil_y_Hat_Prac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()


#Testing things out! - maybe will add to results

matrixEssBetasStan= array(essBetasDrawsStan, dim = c(T,P))

cairo_pdf("Mobil_N_eff_Time_Stan.pdf", width = 10, height = 8)
par(mfrow = c(2,1), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,1], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(alpha[t]), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,2], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(beta[t]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
mtext("Stan Version", outer = TRUE, cex = 1.5, font = 2)
dev.off()


matrixEssBetasDSP = array(essBetasDrawsDsp, dim = c(T,P))
cairo_pdf("Mobil_N_eff_Time_Dsp.pdf", width = 10, height = 8)
par(mfrow = c(2,1), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,1], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(alpha[t]), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,2], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(beta[t]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
mtext("DSP Version", outer = TRUE, cex = 1.5, font = 2)
dev.off()
#Both models have extreme dips on t=25 and t=80, maybe the problem is from the data geometry then from the models!



######## 2nd Experiment  #####
#Fama-French 3 factor!
#use the same data of the 1st experiment, we just add SMB and HML variables (this way we complete the
# fama french model - the data for this 2 extra variables are from the ff 3 factor dataset)
rm(list = ls()[!ls() %in% c("prettycol", "data")])


ff = read.csv("F-F_Research_Data_Factors-SMB_HML.csv")
SMB = ff[ ,2]
HML = ff[ ,3]

####IBM####

y = data$IBM - data$RKFREE
x = data$MARKET - data$RKFREE

X = cbind(Intercept = 1, ExcessMarket = x, SMB, HML)

#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "IBM_Fama_French_4.22min.rds")


modelMyVerFF = readRDS("IBM_Fama_French_4.22min.rds")
modelStanFF = readRDS("fit_IBM_NonCentSV2Exp.rds")

#Regressors
T = nrow(X)
P = ncol(X) 

betaDrawsStan= modelStanFF$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVerFF$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])
meanWidthB3 = mean(widthStan[ ,3])
meanWidthB4 = mean(widthStan[ ,4])

widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])
meanWidthB3Dsp = mean(widthDsp[ ,3])
meanWidthB4Dsp = mean(widthDsp[ ,4])


x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta[1]), bquote(beta[2]), bquote(beta[3]))

PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}

cairo_pdf("alpha_IBM_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("beta_IBM_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("SMB_IBM_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 3, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("HML_IBM_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 4, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStanFF$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVerFF$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

cairo_pdf("Sd_SV_IBM_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

widthStan = hdiSdStan[2, ] - hdiSdStan[1, ]
meanWidthStan = mean(widthStan)

widthDsp = hdiSdDsp[2, ] - hdiSdDsp[1, ]
meanWidthDsp = mean(widthDsp)

###
timeInSec = modelStanFF$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 4 + (22/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

cairo_pdf("IBM_hist_ESS_2ndPrac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
dev.off()

#Ypred
yPredStan = modelStanFF$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVerFF$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("IBM_y_Hat_2ndPrac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()

####DELTA####

rm(list = ls()[!ls() %in% c("prettycol", "data", 'ff', 'SMB', 'HML')])

y = data$DELTA - data$RKFREE
x = data$MARKET - data$RKFREE

X = cbind(Intercept = 1, ExcessMarket = x, SMB, HML)

#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "Delta_Fama_French_4.04min.rds")

modelMyVerFF = readRDS("Delta_Fama_French_4.04min.rds")
modelStanFF = readRDS("fit_DELTA_NonCentSV2Exp.rds")

T = nrow(X)
P = ncol(X) 

betaDrawsStan= modelStanFF$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVerFF$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])
meanWidthB3 = mean(widthStan[ ,3])
meanWidthB4 = mean(widthStan[ ,4])


widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])
meanWidthB3Dsp = mean(widthDsp[ ,3])
meanWidthB4Dsp = mean(widthDsp[ ,4])

x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta[1]), bquote(beta[2]), bquote(beta[3]))

#Betas
PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}

cairo_pdf("alpha_Delta_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("beta_Delta_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("SMB_Delta_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 3, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

cairo_pdf("HML_Delta_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 4, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStanFF$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVerFF$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

widthStan = hdiSdStan[2, ] - hdiSdStan[1, ]
meanWidthStan = mean(widthStan)

widthDsp = hdiSdDsp[2, ] - hdiSdDsp[1, ]
meanWidthDsp = mean(widthDsp)

cairo_pdf("Sd_SV_DELTA_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

#time - comparison
timeInSec = modelStanFF$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 4 + (4/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

#cairo_pdf("DELTA_hist_ESS_2ndPrac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))

hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
abline(v=2800, col = 'red')

#dev.off()

#Ypred
yPredStan = modelStanFF$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVerFF$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)

cairo_pdf("DELTA_y_Hat_Prac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()

####Mobil####

rm(list = ls()[!ls() %in% c("prettycol", "data", 'ff', 'SMB', 'HML')])

y = data$MOBIL - data$RKFREE
x = data$MARKET - data$RKFREE

X = cbind(Intercept = 1, ExcessMarket = x, SMB, HML)

#modelSpec <- dspPerfected::dsp_spec(family = 'gaussian', model = 'regression', X=X, D = 1, evol_error = 'DHS', obsSV = 'SV')
#modelMyDSP <- dspPerfected::dsp_fit(y=y, model_spec = modelSpec, nsave = 12000, nburn = 1000)
#saveRDS(modelMyDSP, "Mobil_Fama_French_4.19min.rds")

modelMyVerFF = readRDS("Mobil_Fama_French_4.19min.rds")
modelStanFF = readRDS("fit_MOBIL_NonCentSV2Exp.rds")

T = nrow(X)
P = ncol(X) 

betaDrawsStan= modelStanFF$draws(variables = 'betas')
niter = dim(betaDrawsStan)[1]
betaDrawsStan = array(betaDrawsStan, dim = c(niter, T, P))

betaDrawsDsp = modelMyVerFF$mcmc_output$beta #We used the modified!



medianBetaStan <- apply(betaDrawsStan, c(2, 3), median) # nQuant x T x P array

medianBetaDsp <- apply(betaDrawsDsp, c(2, 3), median)

hdiBetaStan <- apply(betaDrawsStan, c(2,3), hdi, credMass=0.95)
hdiBetaDsp <- apply(betaDrawsDsp, c(2,3), hdi, credMass=0.95)

#width C.I.
widthStan = hdiBetaStan[2, ,] - hdiBetaStan[1, ,]
meanWidthB1 = mean(widthStan[ ,1])
meanWidthB2 = mean(widthStan[ ,2])
meanWidthB3 = mean(widthStan[ ,3])
meanWidthB4 = mean(widthStan[ ,4])


widthDsp = hdiBetaDsp[2, ,] - hdiBetaDsp[1, ,]

meanWidthB1Dsp = mean(widthDsp[ ,1])
meanWidthB2Dsp = mean(widthDsp[ ,2])
meanWidthB3Dsp = mean(widthDsp[ ,3])
meanWidthB4Dsp = mean(widthDsp[ ,4])



x = seq(from = 1, to = T, by = 1)
colNames = c(bquote(alpha), bquote(beta[1]), bquote(beta[2]), bquote(beta[3]))

#Betas
PlotBetas <- function(medianBetaStan, medianBetaDsp, x , j, hdiStan, hdiDsp, colNames){
  
  lowerlim = min(c(hdiStan[1,,j], hdiDsp[1,,j]))
  upperlim = max(c(hdiStan[2,,j], hdiDsp[2,,j]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = colNames[j])
  
  polygon(c(x, rev(x)), c(hdiStan[1,,j], rev(hdiStan[2,,j])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiDsp[1,,j], rev(hdiDsp[2,,j])),
          col = NA, border = prettycol[6])
  
  
  lines(x, medianBetaStan[ ,j], col = prettycol[1], lwd = 2)
  
  lines(x, medianBetaDsp[ ,j], col = prettycol[2], lwd = 2)
  
  legend(x = 'bottomright', legend = c('Stan Median', 'DSP Median', '95% HPD Stan', '95% HPD DSP'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
  
}


cairo_pdf("alpha_MOBIL_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 1, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


cairo_pdf("Beta_MOBIL_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 2, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


cairo_pdf("SMB_MOBIL_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 3, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()


cairo_pdf("HML_MOBIL_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotBetas(medianBetaStan = medianBetaStan, medianBetaDsp = medianBetaDsp,
          x = x, j = 4, hdiStan = hdiBetaStan, hdiDsp = hdiBetaDsp, colNames = colNames)
dev.off()

PlotSd = function(medianSdStan, medianSdDsp, x, hdiSdStan, hdiSdDsp){
  lowerlim = min(c(hdiSdStan[1, ], hdiSdDsp[1, ]))
  upperlim = max(c(hdiSdStan[2, ], hdiSdDsp[2, ]))
  
  plot(x, rep(NA, length(x)), type = "n", ylab = "", xlab = "Time", 
       ylim = c(lowerlim, upperlim), main = expression(sigma[epsilon]))
  
  polygon(c(x, rev(x)), c(hdiSdStan[1, ], rev(hdiSdStan[2, ])),
          col = adjustcolor(prettycol[5], alpha.f = 0.30), border = NA)
  
  polygon(c(x, rev(x)), c(hdiSdDsp[1, ], rev(hdiSdDsp[2, ])),
          col = NA, border = prettycol[6])
  
  lines(x, medianSdStan, col = prettycol[1], lwd = 2)
  
  lines(x, medianSdDsp, col = prettycol[2], lwd = 2)
  
  legend(x = 'topright', legend = c('Median - Stan', 'Median - Dsp', '95% HPD Stan', '95% HPD Dsp'), 
         col = c(prettycol[1], prettycol[2], NA, prettycol[6]), 
         lwd = c(2, 2, NA, 1), 
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 22, NA),
         pt.bg = c(NA, NA, adjustcolor(prettycol[5], alpha.f = 0.30), NA),
         pt.cex = 2.5, cex = 0.75, seg.len=4
  )
}  
x = seq(from = 1, to = T, by = 1)

sdStan = exp(modelStanFF$draws('htSV')/2)
rows = dim(sdStan)[1]
cols = dim(sdStan)[3]

sdStan = matrix(sdStan, nrow = rows, ncol = cols)
sdDsp = sqrt(modelMyVerFF$mcmc_output$obs_sigma_t2)

medianSdStan = apply(sdStan, 2, median)
medianSdDsp = apply(sdDsp, 2, median)

hdiSdStan = apply(sdStan, 2, hdi, credMass=0.95)
hdiSdDsp = apply(sdDsp, 2, hdi, credMass=0.95)

widthSdStan = hdiSdStan[2, ] - hdiSdStan[1, ]
mean(widthSdStan)

widthSdDsp = hdiSdDsp[2, ] - hdiSdDsp[1, ]
mean(widthSdDsp)


cairo_pdf("Sd_SV_MOBIL_2ndPrac_plotStanDSP.pdf", width = 7, height = 7)
PlotSd(medianSdStan = medianSdStan, medianSdDsp = medianSdDsp, x = x, hdiSdStan = hdiSdStan, hdiSdDsp = hdiSdDsp)
dev.off()

#time - comparison
timeInSec = modelStanFF$time()
totalTimeInSec = timeInSec$total
totalTimeInMinStan = totalTimeInSec / 60

totalTimeInMinDsp = 4 + (19/60)

#ESS
library('coda')
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

xmin = min(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))
xmax = max(c(essBetasDrawsStanPerMin, essBetasDrawsDspPerMin))

cairo_pdf("MOBIL_hist_ESS_2ndPrac_plotStan2xDSP.pdf", width = 10, height = 8)
par(mfrow = c(2,1))
hist(essBetasDrawsStanPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[4], main = 'Stan Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
hist(essBetasDrawsDspPerMin, breaks = 50, freq = FALSE, xlim = c(xmin, xmax), col = prettycol[2], main = 'DSP Version' , ylab = 'Density', xlab = expression(N[eff] ~ "per Minute"))
dev.off()

#Ypred
yPredStan = modelStanFF$draws(variables = 'yPred')
dimsYPred = dim(yPredStan)
nIter = dimsYPred[1] 
yPredStan = array(yPredStan, dim = c(nIter, T))

yPredDsp = modelMyVerFF$mcmc_output$yhat

densYStan = density(yPredStan)
densYDsp = density(yPredDsp)


cairo_pdf("MOBIL_y_Hat_Prac_plotStanDSP.pdf", width = 7, height = 6)
hist(y, breaks = 30, freq = FALSE, col = prettycol[8], plot = TRUE,
     main = '', xlab = 'y Draws')

lines(densYStan$x, densYStan$y, lty = 1, lwd = 2.5, col = prettycol[1])
lines(densYDsp$x, densYDsp$y, lty = 1, lwd = 2.5, col = prettycol[2])

legend(x = 'topright', legend = c('True y','Pred. y - Stan', 'Pred. y - DSP'),
       lty = c(NA, 1, 1), lwd = c(NA, 2, 2), col = c(NA, prettycol[1], prettycol[2]),
       pch = c(22, NA, NA), pt.bg = c(prettycol[8], NA, NA),
       pt.cex = 2, cex = 0.80)
dev.off()


##Test-Mobil peaks

matrixEssBetasStan= array(essBetasDrawsStan, dim = c(T,P))

cairo_pdf("Mobil_N_eff_Time_Stan_2ndexp.pdf", width = 10, height = 8)
par(mfrow = c(2,2), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,1], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(alpha), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,2], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(beta[1]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,3], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(beta[2]), lwd = 2, lty = 1, col = prettycol[2],
     type = 'l')
plot(1:dim(matrixEssBetasStan)[1], matrixEssBetasStan[,4], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsStan*1.2)),main = bquote(beta[3]), lwd = 2, lty = 1, col = prettycol[2],
     type = 'l')

mtext("Stan Version", outer = TRUE, cex = 1.5, font = 2)

dev.off()


matrixEssBetasDSP = array(essBetasDrawsDsp, dim = c(T,P))
cairo_pdf("Mobil_N_eff_Time_Dsp_2ndexp.pdf", width = 10, height = 8)
par(mfrow = c(2,2), oma = c(0, 0, 3, 0))
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,1], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(alpha), lwd = 2, lty = 1, col = prettycol[1],
     type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,2], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(beta[1]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,3], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(beta[2]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
plot(1:dim(matrixEssBetasDSP)[1], matrixEssBetasDSP[,4], xlab = 'Time', ylab = expression(N[eff]), ylim = c(0, max(essBetasDrawsDsp*1.2)), main = bquote(beta[3]), lwd = 2, lty = 1, col = prettycol[2], type = 'l')
mtext("DSP Version", outer = TRUE, cex = 1.5, font = 2)
dev.off()

