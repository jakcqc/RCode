library(R.matlab)
dataSet = readMat("data_class4.mat")
classData = dataSet$Data
##functions##
#plot first two dimension of each class
plotDim2 = function(data, color, sPlot){
  plot(data[,1], data[,2], col = color,xlab="", ylab ="", ylim = range(-5,15), 
      xlim = range(-5,15), axes=sPlot )
  par(new=TRUE)
}
#Get largest eigen vector
getEig = function(covMat, colSize)
{
  baseC = 0
  matExp = covMat
  cVec = rep(1:colSize)
  cVec = cVec / sqrt(sum(cVec^2))
  while(baseC == 0)
  {
    baseVec = cVec
    cVec = covMat %*% cVec
    cVec = cVec / sqrt(sum(cVec^2))
    checker = all.equal(cVec, baseVec)
    if(checker[1] == TRUE)
    {
      baseC = 1
    }
  }
  newEigenVec = cVec
  return(newEigenVec)
}
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
## plot each principle vector
plotPrincipleVectors = function(cVectors, sPlot, cCol)
{
  plot(seq(1,5), cVectors[,1], col = cCol, axes = FALSE, pch = 19, 
       xlim = range(1,5), ylim = range(-2,2))
  lines(seq(1,5),cVectors[,1], col = cCol)

  par(new=TRUE)
  plot(seq(1,5), cVectors[,2], col = cCol, axes = sPlot, pch = 19, 
       xlim = range(1,5), ylim = range(-2,2))
  lines(seq(1,5),cVectors[,2], col = cCol)
  
  par(new=TRUE)
  
  ##lines(cVectors[,2], col = cCol)
}
#hand calculated covariance matrix
calcCovByHand = function(dataSet)
{
  sizes = t(dim(dataSet))
  meansMat <- matrix(data=1, nrow=sizes[,1]) %*% 
    cbind(mean(dataSet[,1]),mean(dataSet[,2]),mean(dataSet[,3]),
          mean(dataSet[,4]), mean(dataSet[,5])) 
  
  difMat <- dataSet - meansMat
  covFMat <- t(difMat) %*% difMat / (sizes[,1]-1)
  return (covFMat)
}
##Converting into Matrix with proper Dimension
classOne <- matrix(unlist(classData[[1]]), ncol = 5, nrow = 50)
classTwo <- matrix(unlist(classData[[2]]), ncol = 5, nrow = 500)
classThree <- matrix(unlist(classData[[3]]), ncol = 5, nrow = 500)
classFour <- matrix(unlist(classData[[4]]), ncol = 5, nrow = 10000)

##Grab the Cov matrix, need to do by hand
classOneCov = cov(classOne)
classTwoCov = cov(classTwo)
classThreeCov = cov(classThree)
classFourCov = cov(classFour)

#"Hand" generated cov matrix
newClassOneCov = calcCovByHand(classOne)
?cov
cov(classOne, method = "kendall")
cov(classOne, method = "spearman")

newClassOneCov
classOneCov
newClassTwoCov = calcCovByHand(classTwo)
newClassTwoCov
classTwoCov
newClassThreeCov = calcCovByHand(classThree)
newClassThreeCov
classThreeCov
newClassFourCov = calcCovByHand(classFour)
newClassFourCov
classFourCov

##make mean of each class, aka get the mean for each class
classOneMean = mean(classOne)
classTwoMean = mean(classTwo)
classThreeMean = mean(classThree)
classFourMean = mean(classFour)
#print class Means
classOneMean
classTwoMean
classThreeMean
classFourMean

##plot first two dimension of each 
##four = pink, three = green, two = red, one = blue
plotDim2(classFour, 'pink',FALSE)
plotDim2(classThree, 'green',FALSE)
plotDim2(classTwo, 'red',FALSE)
plotDim2(classOne, 'blue',TRUE)

plotPrincipleVectors(newClassOneEig,FALSE, "blue")
plotPrincipleVectors(newClassTwoEig,FALSE, "red")
plotPrincipleVectors(newClassThreeEig,FALSE, "green")
plotPrincipleVectors(newClassFourEig,TRUE, "pink")

##Get eigen vector and values
classOneEigen = eigen(classOneCov)
classTwoEigen = eigen(classTwoCov)
classThreeEigen = eigen(classThreeCov)
classFourEigen = eigen(classFourCov)

## eigen Vectors
## calc class one  Eig vector/value 
newClassOneEig = getEigOrtho(classOneCov,5)
newClassOneEig
classOneEigValues = t(c(0,0,0,0,0))
classOneEigValues = getEigenValueFromVec(classOneCov, 
                                        newClassOneEig, 
                                        classOneEigValues, 5)

classOneEigValues
#######
## calc class two  Eig vector/value 

newClassTwoEig = getEigOrtho(classTwoCov,5)
newClassTwoEig
classTwoEigValues = t(c(0,0,0,0,0))
classTwoEigValues = getEigenValueFromVec(classTwoCov, 
                                         newClassTwoEig, 
                                         classTwoEigValues, 5)
classTwoEigValues
###
## calc class three  Eig vector/value 

newClassThreeEig = getEigOrtho(classThreeCov,5)
newClassThreeEig
classThreeEigValues = t(c(0,0,0,0,0))
classThreeEigValues = getEigenValueFromVec(classThreeCov, 
                                         newClassThreeEig, 
                                         classThreeEigValues, 5)
classThreeEigValues
###
## calc class four  Eig vector/value 

newClassFourEig = getEigOrtho(classFourCov,5)
newClassFourEig
classFourEigValues = t(c(0,0,0,0,0))
classFourEigValues = getEigenValueFromVec(classFourCov, 
                                         newClassFourEig, 
                                         classFourEigValues, 5)
classFourEigValues

### lets try and plot the right mu and eigen
newClassOneEig
classOneEigValues

plotDim2(classTwo, "blue", TRUE)
plotterV = function(x)
{
  + classOneEigValues[1]
}
plot(classTwo, col = "red",xlim = range(-5,15), ylim = range(-5,15),)
line(classTwo)
?abline
?line
abline(coef(line(classTwo)))
lines(c(0,classOneEigValues[2]), c(0, classOneEigValues[1]))
vecPlot <- classOneEigValues[1]
par(new = TRUE)
plot(xM, yM, col = "green",xlim = range(-5,15), ylim = range(-5,15), sPlot = FALSE, pch = 19)
par(new = TRUE)
plot(xM + sum(newClassTwoEig[,1]),yM + sum(newClassTwoEig[,2]) , col= "red",
     xlim = range(-5,15), ylim = range(-5,15), sPlot = FALSE)
xMM = mean(classTwo[,1])
yMM = mean(classTwo[,2])

newClassTwoEig
