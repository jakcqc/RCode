##priors P(w1) = 4P(w2)
library(dismo)
mu1 = c(5,1)
mu2 = c(1,7)

sigma1 = matrix(c(3.1,1,1,2.6), nrow = 2)
sigma2 = sigma1

getMahal()


##functions
getMahal <- function(x1, x2, covMat, mu)
{
  x1M = (sqrt(t(x1 - mu) %*% inv(covMat) %*% (x1 - mu)))
  x2M = (sqrt(t(x2 - mu) %*% inv(covMat) %*% (x2 - mu)))
  
  x12M = (sqrt(t(x1 - x2) %*% inv(covMat) %*% (x1 - m2)))
  
  print(x1M)
  print(x2M)
  print(x12M)
  
  
  difMal =abs(x1 -x2)
  print(difMal)
  
}
makePDF <- function()
{
  
}
makeDiscrim <- function()
{
  
}

 
