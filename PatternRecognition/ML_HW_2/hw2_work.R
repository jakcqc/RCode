library(MASS)
library(whitening)
Xo = t(c(.5,0,1))
sigma = matrix(c(1,0,0,0,5,2,0,2,5), nrow = 3, ncol = 3)
mu = t(c(1,2,2))
dSigma = det(sigma)
iSigma = (solve(sigma))
density = 1 / ((2 * pi)^(3/2) * dSigma^.5)
density
tMuXo = t(Xo - mu)
tMuXo
muXo = t(tMuXo)
muXo


density = density *exp ((-.5 * t(Xo - mu)) * iSigma * (Xo - mu))
density * exp(-.5 * 1.06)

vects = (eigen(sigma))
newMV = vects$values^(-.5) * vects$vectors
vects = vects$vectors %*% vects$values^(-.5)
fractions(newMV)
