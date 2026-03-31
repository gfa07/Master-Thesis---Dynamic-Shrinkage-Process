set.seed(123)

T <- 500
P <- 3

betas <- matrix(NA, nrow = T, ncol = P)
for (t in 1:T){
  betas[t,1] = sin(10*t/T)
  betas[t,2] = if (t/T < 0.5) -0.5 else 0.5
  betas[t,3] = (t/T)^2
}
par(mfrow = c(2,2), oma = c(0, 0, 3, 0))
for (j in 1:P){
  plot(betas[,j], type = "l", xlab = 'Time', ylab = bquote(beta[.(j)]))
}
mtext("Evolution of the coefficients", outer = T, cex = 1.5, font = 2)

X <- matrix(rnorm(T*(P-1), mean = 0, sd = 1), nrow = T, ncol = P-1)
X <- cbind('Intercept' = 1, X)

SDSig <- 0.45

y <- numeric(T)
for (t in 1:T){
  y[t] <- sum(X[t, ]*betas[t, ]) + rnorm(1, mean = 0, sd = SDSig)
}
