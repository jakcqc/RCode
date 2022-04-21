##Mini Project five 
#functions 
mahalTwoPoints <- function(x1,x2, sigma)
{
  mahalD = sqrt(t(x1-x2) %*% solve(sigma) %*% (x1 - x2)) 
  return(mahalD)
}
getDiscrim <- function(x, mu, sigma, prior, d) {
  dG = (-0.5*(t(x-mu))%*% solve(sigma) %*%(x - mu)) - ((d/2)*log(2*pi))-(0.5*log(det(sigma)))
  + log(prior)
  return(dG) 
}
mleMean = function(class)
{
  pU = NULL
  for(x in 1:ncol(class))
  {
    pU = append(pU, 0)
    for(z in 1:nrow(class))
    {
      pU[x] = class[z,x] + pU[x]
    }
    pU[x] = pU[x]/nrow(class)
  }
  
  return(t(t(pU)))
  
}
mleVar = function(class, meanVec)
{
  pS = NULL
  
  for(x in 1:ncol(class))
  {
    pS = append(pS, 0)
    for(z in 1:nrow(class))
    {
      pS[x] = (class[z,x]- meanVec[x])^2 + pS[x]
    }
    pS[x] = pS[x]/nrow(class)
  }
  
  
  return(t(t(pS)))
  
}

mleSigma = function(class, meanVec)
{
  pS = matrix(0,ncol(class), ncol(class))
  for(z in 1:nrow(class))
  {
    pS = (class[z,]- meanVec)%*%t((class[z,]- meanVec)) + pS
  }
  pS = pS*(1/(nrow(class)))
  
  return(pS)
  
}
getDecisionBound <-  function(cClassMean, cClassSigma, cClassMean2, cClassSigma2, x,y)
{
  class1D = matrix(0 , nrow = 13, ncol = 13)
  classPDF = matrix(0 , nrow = 13, ncol = 13)
  
  classOnly = c()
  classOnlyMat = matrix(0 , nrow = 13, ncol = 13)
  
     curX = getDiscrim(t(t(c(x,y))), cClassMean, cClassSigma, .5, 2)
    # classOnlyMat[zz,x] = curX
     curX = curX -  getDiscrim(t(t(c(x,y))), cClassMean2, cClassSigma2, .5, 2)
    #class1D[x,y] =  curX
     return(curX)
    classOnly = append(classOnly, curX)
    #classPDF[x,zz]= (1/(2 * pi)*det(cClassSigma)^.5) * exp(mahalTwoPoints(t(t(c(x,zz))), cClassMean, cClassSigma))
                   
  
  #return(classPDF)
  #return(classOnly)
 #return(classOnlyMat)
 return(class1D)
}
x <- y <- seq(-10, 21)
ranger = seq(-10,21)
z = getDecisionBound(u1,s1,u2,s2)
zSorted = sort(z,decreasing = TRUE)

plot(ranger, zSorted)
persp(x, y, zSorted,theta = 20, phi = 45)
persp(x, y, z,theta = -60, phi = 50, shade = "0.8")
persp(x, y,z, theta = -150, phi = 80, shade = "0.8")
z = matrix(.5, nrow = 32, ncol = 32)
plot(ranger, z)


#This question is to demonstrate that: 
#1) when the selected model is poor, the maximum-likelihood classifier does not produce
#satisfactory results; and 
#2) proper transformation of the data can compensate for poor models. The dataset1 used for this
#question is divided into training2 and test3 data, with each one consisting of 3 classes in a 2D-feature space.

#a) Assuming Gaussian distribution for all three class conditional densities, and with unknown means and covariances,
#compute the maximum likelihood estimates for each class using the training data.


testClass1 <-t(t(read.csv("MP5-data-csv/test_class_1.csv")))


testClass2 <- t(t(read.csv("MP5-data-csv/test_class_2.csv")))


testClass3 <- t(t(read.csv("MP5-data-csv/test_class_3.csv")))


trainClass1 <- t(t(read.csv("MP5-data-csv/train_class_1.csv")))
trainClass1Mean = mleMean(trainClass1)
trainClass1Sigma = mleSigma(trainClass1, trainClass1Mean)

trainClass2 <- t(t(read.csv("MP5-data-csv/train_class_2.csv")))
trainClass2Mean = mleMean(trainClass2)
trainClass2Sigma = mleSigma(trainClass2, trainClass2Mean)

trainClass3 <- t(t(read.csv("MP5-data-csv/train_class_3.csv")))
trainClass3Mean = mleMean(trainClass3)
trainClass3Sigma = mleSigma(trainClass3, trainClass3Mean)

##Mini project 2, 1a and 1b 
plot(trainClass1Mean[1,],trainClass1Mean[2,], col = "red" , xlim = range(-5,5), ylim = range(-5,5), pch = 20)
points(trainClass2Mean[1,], trainClass2Mean[2,], col = "blue", pch = 20)
points(trainClass3Mean[1,], trainClass3Mean[2,], col = "green", pch= 20)


#plotting discrim now 
ranger = seq(-5, 5, by = .001)


class12Bound = getDecisionBound(trainClass1Mean, trainClass1Sigma, trainClass2Mean, trainClass2Sigma)
lines(ranger,class12Bound, col= "purple")

class13Bound = getDecisionBound(trainClass1Mean, trainClass1Sigma, trainClass3Mean, trainClass3Sigma)
lines(ranger,class13Bound, col= "pink")

class23Bound = getDecisionBound(trainClass2Mean, trainClass2Sigma, trainClass3Mean, trainClass3Sigma)
lines(ranger,class23Bound, col= "brown")

confMat = matrix(0, nrow= 3, ncol = 3)

getConf = function(class)
{
  errorM = c(0,0,0)
  polarC = matrix(0,nrow = nrow(class), ncol= 2)
  for(x in 1:nrow(class))
  {
    point = t(t(class[x,]))
    d12 = getDecisionBound(trainClass1Mean, trainClass1Sigma,trainClass2Mean,trainClass2Sigma, point[1,],point[2,])
    d23 = getDecisionBound(trainClass2Mean, trainClass2Sigma,trainClass3Mean,trainClass3Sigma, point[1,],point[2,])
    d21 = getDecisionBound(trainClass2Mean, trainClass2Sigma,trainClass1Mean,trainClass1Sigma, point[1,],point[2,])
    
    d13 = getDecisionBound(trainClass1Mean, trainClass1Sigma,trainClass3Mean,trainClass3Sigma, point[1,],point[2,])
    #d31 = getDecisionBound(trainClass3Mean, trainClass3Sigma,trainClass1Mean,trainClass1Sigma, point[1,],point[2,])
    if(d12 > 0 && d13 >0)
    {
      errorM[1] = errorM[1] + 1
    }
    else if(d23 > 0 &&  d21 > 0)
    {
      errorM[2] = errorM[2] + 1
    }
    else{errorM[3]= errorM[3]+1}
  }
 # return(errorM)
  
  return(errorM * (1/x))
}
confMat[1,] = getConf(testClass1)
confMat[2,] = getConf(testClass2)
confMat[3,] = getConf(testClass3)
confMat
trainClass1Mean
trainClass1Sigma

trainClass2Mean
trainClass2Sigma

trainClass3Mean
trainClass3Sigma
##testing functions 
u1 = t(t(c(1.833333,1.5)))
s1 = matrix(c(2.1666666666,1.1,1.1,1.1), nrow = 2)
s1
u2 = t(t(c(8.16666666666,9.33333333)))
u2
s2 = matrix(c(1.36666666,-0.06666666,-0.0666666666,1.066666666), nrow =2)
s2
ranger = seq(-100, 110)
xySeq = c()
boundEX = getDecisionBound(u1, s1, u2,s2)

plot(0,0, pch = 20, col = "red", ylim = range(-100,110), xlim = range(-100, 110))
points(8,11, pch = 20, col = "blue")
points(0,0, pch = 20, col = "red")

points(ranger,boundEX[1,], col = "black", pch = 20 )


#testing from book
u1 = t(t(c(3,6)))
s1 = matrix(c(.5,0,0,2), nrow = 2)
s1
u2 = t(t(c(3,-2)))
u2
s2 = matrix(c(2,0,0,2), nrow =2)
s2



#b) Ignore the priors, i.e. assume 1/3 for all three classes, and redo parts a) and b) of MiniProject 2, Question 1, and use
#those same Matlab functions in part c) below. Use the means and variances from part a) above.

#c) Classify the test data and compute the test error using confusion matrix4.

#d) Bayesian estimates. The data has a simpler description when seen in polar coordinates. Use cart2pol() to transform all
#the data points to polar coordinates. Use scatter() to plot the transformed points. What you should find is that the transformed
#data looks Gaussian on the radius r and uniform on the angle θ. So, ignore the angle θ and classify the test data only on r as
#follows.
getPolar = function(x,y)
{
  r =  sqrt( x^2 + y^2 )
  theta = atan( y / x )
  return(t(c(r,theta)))
}

polarC1 = matrix(0,nrow = nrow(trainClass1), ncol= 2)

for(x in 1:nrow(trainClass1))
{
  temp =  getPolar(trainClass1[x,1], trainClass1[x,2])
  polarC1[x,] = temp
}
polarC2 = matrix(0,nrow = nrow(trainClass2), ncol= 2)

for(x in 1:nrow(trainClass2))
{
  polarC2[x,] = getPolar(trainClass2[x,1], trainClass2[x,2])
}
polarC3 = matrix(0,nrow = nrow(trainClass3), ncol= 2)

for(x in 1:nrow(trainClass3))
{
  polarC3[x,] = getPolar(trainClass3[x,1], trainClass3[x,2])
}
polarC1
polarC2
polarC3
library(pracma)
polar(polarC1[,2],polarC1[,1], type= 'p')
polar(polarC2[,2],polarC2[,1], type= 'p')
polar(polarC3[,2],polarC3[,1], type= 'p')

plot(polarC1[,1],polarC1[,2], col = "red", pch = 0, xlim = range(-3,5), ylim = range(-3,5))
points(polarC2[,1],polarC2[,2], col = "blue", pch = 0, )
points(polarC3[,1],polarC3[,2], col = "green", pch = 0, )


#The problem is now 1-D and again, if you inspect the data, Gaussian distribution is a more suitable pdf to describe all three
#classes. Assume then that each class has p(r|ωi) = Nµi,σ2 with µi unknown and variance σ2 = 0.23 for all three classes. Let
#the only prior knowledge about µi be p(µi) = Nµ0 = 0, σ20 = 110
#and compute the Bayes estimates for µi and the posterior
#distribution p(µi|Di) of all three classes. Next compute p(r|ωi,Di) = ´(Integral) p(r|µi) p(µi|Di)dµi and use 
#this density estimate to classify the test data and compute the test error using confusion matrix.
n1 =  nrow(trainClass1)
n2 = nrow(trainClass2)
n3 = nrow(trainClass3)

predMu1 = sum(polarC1[,1])/n1
predMu2 = sum(polarC2[,1])/n2
predMu3 = sum(polarC3[,1])/n3


bayesMu = function(n, var0, mu0, predMu)
{
  mu = (n * var0)/(n * var0 + .23) * predMu + (.23 / (n * var0 + .23))*mu0
  return(mu)
}
bayesVar = function(n, var0)
{
  return((var0 * .23)/(n *var0 + .23))
}
pUD = function(muN, varN, )

bMu1 = bayesMu(n1,110,0,predMu1)
bMu2 = bayesMu(n2,110,0,predMu2)
bMu3 = bayesMu(n3,110,0,predMu3)
bVarn1 = bayesVar(n1, 110)
bVarn2 = bayesVar(n2, 110)
bVarn3 = bayesVar(n3, 110)

bVarn1
bVarn2
bVarn3


points(bMu1[1], 0, col = 'red', pch = 20)
points(bMu2[1], 0, col = 'blue', pch = 20)
points(bMu3[1], 0, col = 'green', pch = 20)




#Do not forget to comment on the results from each step above.

