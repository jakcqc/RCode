##libs
library(MASS)
library(vegan)
library(plot3D)
##functions 
#QR decomposition to solve for 5 eigen vectors
getEigOrtho = function(covMat, colSize)
{
  Q = calcQMatrix(covMat, colSize)
  counterNew = 0
  while(counterNew < 50000)
  {
    baseQ = Q
    cMat = covMat %*% Q
    Q = calcQMatrix(cMat, colSize)
    checker = all.equal(Q, baseQ )
    if(checker[1]==TRUE)
    {
      print(counterNew)
      counterNew = 100000000
    }
    counterNew = counterNew + 1
  }
  
  return(Q)
}
## finding Q decom at each iteration for eigen vector algorithm
calcQMatrix = function(cMat, colSize)
{
  cont = 1
  calcW = 2
  while(cont <= colSize)
  {
    if(cont == 1)
    {
      cU = cMat[,1]
      eMat = matrix(cU / sqrt(sum(cU^2)), ncol =1)
    }
    if(cont > 1)
    {
      cStep = cont - 1
      cA = cMat[,cont]
      cU = cA - eMat[,cStep]*(cA %*% eMat[,cStep])
      cStep = cStep-1
      while(cStep > 0)
      {
        cU = cU - eMat[,cStep]*(cA %*% eMat[,cStep])
        cStep = cStep - 1
      }
      newCol = matrix(cU / sqrt(sum(cU^2)), ncol = 1)
      eMat = cbind(eMat, newCol)
      
    }
    cont = cont + 1
  }
  return (eMat)
  
}
#eigen values from each vector
getEigenValueFromVec = function(covMat, eigenMat, cClassValue, colSize)
{
  if(colSize == 0)
  {
    return(cClassValue)
  }
  currentAX = covMat %*% eigenMat[,colSize]
  cEigenValue = (currentAX / eigenMat[,colSize])[1]
  cClassValue[,colSize] = cEigenValue
  print(cClassValue)
  getEigenValueFromVec(covMat, eigenMat, cClassValue, colSize-1)
  
}

mahalTwoPoints <- function(x1,x2, sigma)
{
  mahalD = sqrt(t(x1-x2) %*% solve(sigma) %*% (x1 - x2))
  return(mahalD)
}

getDiscrim <- function(x, mu, sigma, prior, d)
{
  dG = -0.5*(t(x-mu))%*% solve(sigma)%*%(x - mu) 
       - (d/2)*log(2*pi)-0.5*log(det(sigma))
       + log(prior)
  
  return(dG)
}
generateSample <- function(eigMat, eigVal, d, n, mu)
{
  testX <- matrix(rnorm(d * n), n)
  
  eigValDiag1 = diag(sqrt(c(eigVal[,1],eigVal[,2])), d)
  
  testX = mu + eigMat %*% eigValDiag1 %*% t(testX)
  
  return(t(testX))
  
}

##set mus 
mu1 = c(5,1)
mu2 = c(1,7)

##set sigma 
sigma1 = sigma2 = matrix(c(3.1,1,1,2.6), nrow = 2)

##testing

mahalTwoPoints(c(5.1,1.1), c(4.9,.9), sigma1)

## graphing ?
####

##get eigen 
eigMat1 = getEigOrtho(sigma1, 2)
eigMat2 = getEigOrtho(sigma2, 2)

##get lambda values ie - eigen values
eigOneVal = t(c(0,0))
eigOneVal = getEigenValueFromVec(sigma1,eigMat1,eigOneVal, 2)

eigTwoVal = t(c(0,0))
eigTwoVal = getEigenValueFromVec(sigma2,eigMat2,eigTwoVal, 2)

###generate sample data
dis1T = generateSample(eigMat1, eigOneVal, 2, 200, mu1)
dis2T = generateSample(eigMat2, eigTwoVal, 2, 800, mu2)


##bivariate normal distribution generation 
plot(dis1T, xlim = range(-6,15), ylim = range(-4,12), col = "green")
points(dis2T, col = "red")



##3d plotting 
count = dim(dis1T)[1]

dis1Z = NULL
dis2Z = NULL
getDiscrim(dis1T, mu1, sigma1, .2, 2)

while(count > 0)
{
  dis1Z = append(dis1Z, getDiscrim(dis1T[count,], mu1, sigma1, .2, 2))
  count = count - 1
}
count = dim(dis2T)[1]
while(count > 0)
{
  dis2Z = append(dis2Z, getDiscrim(dis2T[count,], mu2, sigma2, .2, 2))
  count = count - 1
}
dBound = dis1Z - dis2Z

dis1P = dnorm(seq(-5,13, length = 1099), mu1, sigma1)
plot(seq(-5,13, length = 1099),dis1P)

dis2P = dnorm(seq(-8,14, length = 1099), mu2, sigma2)
plot(seq(-7,13, length = 1099),dis2P)
points(seq(-7,13, length = 1099), dis1P, col = "red", pch =19)

## rest of project

##set new sigma 
sigma1 = sigma2 = matrix(c(3.1,0,0,2.6), nrow = 2)
##get eigen 
eigMat1 = getEigOrtho(sigma1, 2)
eigMat2 = getEigOrtho(sigma2, 2)
##get lambda values ie - eigen values
eigOneVal = t(c(0,0))
eigOneVal = getEigenValueFromVec(sigma1,eigMat1,eigOneVal, 2)

eigTwoVal = t(c(0,0))
eigTwoVal = getEigenValueFromVec(sigma2,eigMat2,eigTwoVal, 2)

eigMat1 = matrix(c(-1,0,0,-1), nrow = 2)
eigOneVal = t(c(3.1,2.6))
dis1T = generateSample(eigMat1, eigOneVal, 2, 200, mu1)
dis2T = generateSample(eigMat1, eigOneVal, 2, 800, mu2)

plot(dis1T, xlim = range(-2,10), ylim = range(-2,10), col = "red")
points(dis2T, col = "blue")

sigma1 =  matrix(c(2.1,1.5,1.5,3.8), nrow = 2)
sigma2 = matrix(c(3.1,.35,.35,2.6), nrow = 2)
##get eigen 
eigMat1 = getEigOrtho(sigma1, 2)
eigMat2 = getEigOrtho(sigma2, 2)
##get lambda values ie - eigen values
eigOneVal = t(c(0,0))
eigOneVal = getEigenValueFromVec(sigma1,eigMat1,eigOneVal, 2)

eigTwoVal = t(c(0,0))
eigTwoVal = getEigenValueFromVec(sigma2,eigMat2,eigTwoVal, 2)

dis2T = generateSample(eigen(sigma2)$vectors, t(eigen(sigma2)$values), 2, 660, mu1)
dis1T = generateSample(eigen(sigma1)$vectors, t(eigen(sigma1)$values), 2, 330, mu1)

plot(dis1T, xlim = range(-2,10), ylim = range(-2,10), col = "red")
points(dis2T, col = "blue")


