#install.packages


prettycol = c("#6C8EBF", "#c0a34d", "#780000","#007878","#B5C6DF","#EADAAA","#AE6666")
x = seq(from = -5, to = 5, length=1000)
y = seq(from = -5, to = 5, length=1000)

plot(x,y, type = 'n')



nu = seq(from = 0, to =2*pi, length=500)
radius = 1

xCords <- radius * cos(nu)
yCords <- radius * sin(nu)

cairo_pdf("hamiltonianDynamics.pdf", width = 7, height = 5)

plot(xCords, yCords, type = "l", lwd = 2, col = prettycol[1], xlim = c(-2,2), ylim = c(-2,2),
     xlab = expression(theta), ylab = expression(phi))


xCordsLines = numeric(500)
yCordsLines = numeric(500)

r2 <- seq(1, 1.8, length=500)
xCordsLines <- r2 * cos(nu)
yCordsLines <- r2 * sin(nu)


lines(xCordsLines, yCordsLines, col = prettycol[3], lwd=2)

I2 = seq(from = 2, to = 500, by = 20)
xCordsPoints = numeric(length(I2))
yCordsPoints = numeric(length(I2))

xCordsPoints = xCordsLines[I2]
yCordsPoints = yCordsLines[I2]

points(xCordsPoints, yCordsPoints, pch = 16, col = prettycol[3])

legend(x = 'topright', legend = c('True Hamiltonian dynamics', 'Simulated Hamiltonian dynamics'),
       col = c(prettycol[1], prettycol[3]), lwd =c(2,2))

dev.off()