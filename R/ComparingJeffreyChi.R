set.seed(123)
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666",
              "#9E4F4F", "#2F4858", "#7A6C5D","#3A7D7D")

sigma = seq(from = 0.0001, to = 2, length.out = 2000)

jeff = 1/sigma

distanceSigmaGrid = diff(sigma)[1]

#normalizing -> make the density function integrates to 1
jeffDens = jeff / (sum(jeff)*distanceSigmaGrid)


scaledInvChi = function(x, nu, sigma){
  return((((nu/2)^(nu/2))/gamma(nu/2))*(sigma^nu)*x^-((nu/2)+1)*exp(-0.5*nu*sigma^2*(1/x)))
}

scaledInvChiC = scaledInvChi(x=sigma, nu = 0.00001, sigma = 0.00001)

scaledInvChiCDens = scaledInvChiC / (sum(scaledInvChiC)*distanceSigmaGrid)


plot(sigma, scaledInvChiCDens, type = 'l', lwd = 2,
     xlab = expression(sigma[epsilon]),
     ylab = "Density",
     main = expression("Jeffreys vs Scaled Inv-" * chi^2),
     col = prettycol[1], ylim = c(0, 50))

lines(sigma, jeffDens, lwd = 2, col = prettycol[2], lty = 2)

legend(x = 'topright', legend =c('Jeffreys', expression("Scaled Inv-" * chi^2)), lwd = c(2,2), 
       lty = c(1,2), col = c(prettycol[2], prettycol[1]))

       