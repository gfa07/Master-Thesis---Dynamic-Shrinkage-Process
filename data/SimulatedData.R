set.seed(123)

T <- 500
P <- 3

betas <- matrix(NA, nrow = T, ncol = P)
for (t in 1:T){
  betas[t,1] = sin(10*t/T)
  betas[t,2] = if (t/T < 0.5) -0.5 else 0.5
  betas[t,3] = (t/T)^2
}

cairo_pdf("betas_plot.pdf", width = 7, height = 3)

par(mfrow = c(1,3), mar = c(4.5, 5, 2, 1), mgp = c(2.5, 1, 0)) 

for (j in 1:P){
  plot(betas[,j], type = "l", lwd = 2, xlab = 'TimeStep', ylab = bquote(beta[.(j)]),
       cex.lab = 1.4, cex.axis = 1.2)
}
dev.off()

X <- matrix(rnorm(T*(P-1), mean = 0, sd = 1), nrow = T, ncol = P-1)
X <- cbind('Intercept' = 1, X)

SDSig <- 0.45

y <- numeric(T)
for (t in 1:T){
  y[t] <- sum(X[t, ]*betas[t, ]) + rnorm(1, mean = 0, sd = SDSig)
}
