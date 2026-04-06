set.seed(123)

prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")


T <- 10000 
betas <- matrix(NA, ncol = T, nrow=2)
sdts <- c(1,10)
row <- c(1,2)

for(j in row){
  betas[j,1] <- 0
  for(i in 2:T){
    innovation <- rnorm(1, mean= 0, sd=sdts[j]) 
    betas[j,i] <- betas[j,i-1] + innovation
      
    }
  
}

yMax <- max(abs(betas))

plot( betas[1,], type = "l", lwd = 2, xlab = "TimeStep", ylab = expression(beta[t])
      , col = prettycol[1], ylim=c(-yMax, yMax))

lines(betas[2,], type = "l", lwd = 2, col = prettycol[2])

legend('topright', legend = paste0('Variance = ', sdts^2), col = prettycol[1:2], lwd = 2)

