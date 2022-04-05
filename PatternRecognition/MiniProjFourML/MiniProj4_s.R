## For Grad Students: Computer exercise 1 and 2 on page 156, in chapter 3 of Duda’s textbook.
# For Undergrad Students: Computer exercise 1 and 10 on page 156 and 158 (respectively), in chapter
# 3 of Duda’s textbook.

#data! 
#make overall data class
classData = t(matrix(c(0.42,-0.087,0.58,-0.4,0.58,0.089,0.83,1.6,-0.014
 ,-0.2,-3.3,-3.4,-0.31,0.27,-0.04,1.1,1.6,0.48
 ,1.3,-0.32,1.7,0.38,0.055,-0.035,-0.44,-0.41,0.32
 ,0.39,0.71,0.23,-0.15,0.53,0.011,0.047,-0.45,1.4
 ,-1.6,-5.3,-0.15,-0.35,0.47,0.034,0.28,0.35,3.1
 ,-0.029,0.89,-4.7,0.17,0.69,0.1,-0.39,-0.48,0.11
 ,-0.23,1.9,2.2,-0.011,0.55,-0.18,0.34,-0.079,0.14
 ,0.27,-0.3,-0.87,-0.27,0.61,0.12,-0.3,-0.22,2.2
 ,-1.9,0.76,-2.1,-0.065,0.49,0.0012,1.1,1.2,-0.46
 ,0.87,-1.0,-2.6,-0.12,0.054,-0.063,0.18,-0.11,-0.49), nrow = 9, ncol = 10))

 #assign each data to its specific class
w1 = classData[,c(1,2,3)]
w2 = classData[,c(4,5,6)]
w3 = classData[,c(7,8,9)]
 
 calcCovDim3 = function(dataSet)
 {
   sizes = t(dim(dataSet))
   meansMat <- matrix(data=1, nrow=sizes[,1]) %*% 
     cbind(mean(dataSet[,1]),mean(dataSet[,2]),mean(dataSet[,3]))
   
   difMat <- dataSet - meansMat
   covFMat <- t(difMat) %*% difMat / (sizes[,1]-1)
   return (covFMat)
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
 ##problem 1.
# Consider Gaussian density models in diﬀerent dimensions.
# (a) Write a program to ﬁnd the maximum likelihood values ˆµ and ˆσ2.
# Apply your program individually to each of the three features
# xi of category ω1 in the table above.
#
#WORK BELOW
# we define our sample mean or maximum likelihood estimate as 
#  = 1/n * sum k = 1 to n for ( x sub k)
#
#our mle for ^σ2 is = 1/n * sum k = 1 to n for (x sub k - ^u)^2
#
# our sample sigma or MLE for Sigma 
# = 1/ n-1 * sum k = 1 to n for 
# ((x sub k - ^u sub n) * transpose(x sub k - ^u sub n))
# 
#CODE

w1MeanVec = mleMean(w1)
w1VarVec = mleVar(w1,w1MeanVec)
w1Sig = mleSigma(w1,w1MeanVec)
w1SigMin = calcCovDim3(w1)
w1MeanVec
w1VarVec
w1Sig
w1SigMin

#mle mean x1
w1MeanVec[1]
#mle mean x2
w1MeanVec[2]
#mle mean x3
w1MeanVec[3]

#mle var2 x1
w1VarVec[1]
#mle var2 x2
w1VarVec[2]
#mle var2 x3
w1VarVec[3]


# (b) Modify your program to apply to two-dimensional Gaussian data p(x)∼N(µ,Σ).
# Apply your data to each of the three possible pairings of two features for ω1.
#
# MLEmean for x1 and x2
w1MeanVec[c(1,2)]
# MLEmean for x1 and x3
w1MeanVec[c(1,3)]
# MLEmean for x2 and x3
w1MeanVec[c(2,3)]

# MLESigma for x1 and x2
w1Sigmax1x2 = mleSigma(w1[,c(1,2)],w1MeanVec[c(1,2)])
w1Sigmax1x2
# MLESigma for x1 and x3
w1Sigmax1x3 = mleSigma(w1[,c(1,3)],w1MeanVec[c(1,3)])
w1Sigmax1x3
# MLESigma for x2 and x3
w1Sigmax2x3 = mleSigma(w1[,c(2,3)],w1MeanVec[c(2,3)])
w1Sigmax2x3
# (c) Modify your program to apply to three-dimensional Gaussian data. 
# Apply your data to the full three-dimensional data for ω1.

#MLEmean x1 x2 x3
w1MeanVec
#MLEVar x1 x2 x3
w1Sig

# (d) Assume your three-dimensional model is separable, so that
# Σ = diag(σ^2 sub 1,σ^2 sub 2,σ^2 sub 3).
# Write a program to estimate the mean and the diagonal components of Σ.
# Apply your program to the data in ω2

w2MeanVec = mleMean(w2)
w2VarVec = mleVar(w2,w2MeanVec)
w2Sigma = mleSigma(w2, w2MeanVec)


#MLe mean w2
w2MeanVec

#MLE var x1, x2, x3
w2VarVec


 # (e) Compare your results for the mean of each feature µi calculated 
# in the above ways. Explain why they are the same or diﬀerent.

# (f) Compare your results for the variance of each feature σ2i
# calculated in the above ways. Explain why they are the same or diﬀerent.




#problem 10
# Suppose we know that the ten data points in category ω2
# in the table above come from a three-dimensional uniform distribution
# p(x|ω2)∼ U(xl,xu). 
# a Suppose,however, that we do not have access to the 
# x3 components for the even-numbered data points.
#
# (a) Write an EM program to estimate the six scalars comprising
# xl and xu of the distribution. Start your estimate with
# xl = (−2,−2,−2)t and x u= (+2,+2,+2)t. 
#
# (b) Compare your ﬁnal esimate with that for the case when there is no missing data.\



sLow = c(-2,-2,-2)
sHigh = c(2,2,2)

sLow2 = c(10,10,10)
sHigh2 = c(-10,-10,-10)

#calc the MLE for all data
for (x in 1:nrow(w2)) 
{
  for(z in 1:ncol(w2))
  {
    if(w2[x,z] < sLow2[z])
    {
      sLow2[z] = w2[x,z]
    }
    if (w2[x,z] >  sHigh2[z])
    {
      sHigh2[z] = w2[x,z]
    }
  }
}
sLow2
sHigh2

#calc with no 3 when even 
#calc the MLE for all data
sLow3 = c(10,10,10)
sHigh3 = c(-10,-10,-10)
for (x in 1:nrow(w2)) 
{
  for(z in 1:ncol(w2))
  {
    if(z == 3 && x%%2 == 0)
    {
      print('do none')
    }
    else
    {
      if(w2[x,z] < sLow3[z])
      {
        sLow3[z] = w2[x,z]
      }
      if (w2[x,z] >  sHigh3[z])
      {
        sHigh3[z] = w2[x,z]
      }
    }
  }
}
sLow3
sHigh3
sLow2
sHigh2
