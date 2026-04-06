#Approximate PG - Xi


library('BayesLogit')
prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")
set.seed(123)

n = 10000
b = 1
c = 0
lim = c(0,4)

xi = rpg(num = n, h = b, z = c)

cairo_pdf("PGvsLogNormal.pdf", width = 7, height = 4.5)

par(mar = c(4.5, 5, 2, 1), mgp = c(2.5, 1, 0))

hist(xi, 30, prob = TRUE, col = prettycol[6],
     lwd = 0.5, ylim=lim, main = '', ylab = 'Density', xlab = expression('Draws of' ~ xi), cex.lab = 1.3,
     cex.axis = 1.1)

dens = density(xi)
lines(x=dens$x, y=dens$y, col = prettycol[1], lwd = 2)

x = seq(from = 0, to = 2, length.out = 1000)
mu = mean(log(xi))
sigma = sd(log(xi))
densLogNorm = dlnorm(x, meanlog = mu, sdlog = sigma)
lines(x=x, y = densLogNorm, col = prettycol[3], lwd = 2 )

legend(x = 'topright', legend = c('Pólya-Gamma Distribution', 'Log-Normal Distribution'), lwd = c(2,2), 
       lty=c(1,1),col= c(prettycol[1],prettycol[3]), cex = 0.75)

dev.off()

mu
sigma

