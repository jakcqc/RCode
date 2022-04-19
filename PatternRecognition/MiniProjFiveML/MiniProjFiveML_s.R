##Mini Project five 
#functions 
mahalTwoPoints <- function(x1,x2, sigma)
{
  mahalD = sqrt(t(x1-x2) %*% solve(sigma) %*% (x1 - x2)) 
  return(mahalD)
}
getDiscrim <- function(x, mu, sigma, prior, d) {
  dG = -0.5*(t(x-mu))%*% solve(sigma)%*%(x - mu) - (d/2)*log(2*pi)-0.5*log(det(sigma))
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
plot(testClass1[,1],testClass1[,2], col = "red" , xlim = range(-5,5), ylim = range(-5,5), pch = 20)
points(testClass2[,1], testClass2[,2], col = "blue", pch = 20)
points(testClass3[,1], testClass3[,2], col = "green", pch= 20)

#b) Ignore the priors, i.e. assume 1/3 for all three classes, and redo parts a) and b) of MiniProject 2, Question 1, and use
#those same Matlab functions in part c) below. Use the means and variances from part a) above.

#c) Classify the test data and compute the test error using confusion matrix4.

#d) Bayesian estimates. The data has a simpler description when seen in polar coordinates. Use cart2pol() to transform all
#the data points to polar coordinates. Use scatter() to plot the transformed points. What you should find is that the transformed
#data looks Gaussian on the radius r and uniform on the angle θ. So, ignore the angle θ and classify the test data only on r as
#follows.

#The problem is now 1-D and again, if you inspect the data, Gaussian distribution is a more suitable pdf to describe all three
#classes. Assume then that each class has p(r|ωi) = Nµi,σ2 with µi unknown and variance σ2 = 0.23 for all three classes. Let
#the only prior knowledge about µi be p(µi) = Nµ0 = 0, σ20 = 110
#and compute the Bayes estimates for µi and the posterior
#distribution p(µi|Di) of all three classes. Next compute p(r|ωi,Di) = ´(Integral) p(r|µi) p(µi|Di)dµi and use 
#this density estimate to classify the test data and compute the test error using confusion matrix.

#Do not forget to comment on the results from each step above.

