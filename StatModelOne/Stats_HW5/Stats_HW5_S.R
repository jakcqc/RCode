## Homework5
library(boot)


Default <- read.csv("Default.csv", stringsAsFactors = TRUE)
Default <- Default[,-1] # Remove the first index column
summary(Default)
## problem 1 
#a)
IBDLine = glm(Default$default ~ Default$balance + Default$income, data = Default, family = binomial)
summary(IBDLine)

#b) 

IBDprob = data.frame(probs = predict(IBDLine, type="response"))
IBDprob =  predict(IBDLine, type="response")

getError(IBDLine, Default, IBDprob)

set.seed(1)

sDefault_default = sample(Default$default, size = 7000)
sDefault_student = sample(Default$student, size = 7000)
sDefault_balance = sample(Default$balance, size = 7000)
sDefault_income = sample(Default$income, size = 7000)

tDefault_default = sample(Default$default[7000:1000], size = 3000)
tDefault_student = sample(Default$student[7000:1000], size = 3000)
tDefault_balance = sample(Default$balance[7000:1000], size = 3000)
tDefault_income = sample(Default$income[7000:1000], size = 3000)


set.seed(1)

##make container class for new sample data 
crossData=sample(1:nrow(Default),size=0.3*nrow(Default))
##generate train data , as -crossData gets 1- ,3, for .7 sample size
trainData=Default[-crossData,]
##default test is .3 so do not need different index
testData=Default[crossData,]

head(trainData)
head(testData)

crossIBDLine = glm(trainData$default ~ trainData$balance + trainData$income, data = trainData, family = binomial)
summary(crossIBDLine)

cIBDprob = data.frame(probs = predict(crossIBDLine, type="response", newdata = testData))
ncIBDprob =  predict(crossIBDLine, type="response", newdata = testData)

getError(crossIBDLine, testData, ncIBDprob[1:3000])

getError <- function(line, data, probD){
  areDef = 0
  areNotDef = 0
  count = 1
  TT = 0
  TF = 0
  FT = 0
  FF = 0
  for(x in probD)
  {
    if(x > .5)
    {
      areDef = areDef + 1
      if(data$default[count] == "No")
      {
        FT = FT + 1
      }
      if(data$default[count] == "Yes")
      {
        TT = TT + 1
      }
    }
    if(x<.5)
    {
      areNotDef = areNotDef + 1
      if(data$default[count] == "No")
      {
        TF = TF + 1
      }
      if(data$default[count] == "Yes")
      {
        FF = FF + 1
      }
    }
    count = count + 1
  }
  conMat = matrix(c(TT,FF,FT, TF), nrow = 2)
  print(conMat)
  
  totalRight = conMat[1,1] + conMat[2,2]
  totalWrong = conMat[1,2] + conMat[2,1]
  error = 1 - totalRight/ (totalRight + totalWrong)
  print(error)
}






#problem 2 
Auto <- read.csv("Auto.csv", na.strings ="?") # With the option, R recognizes ? as NA.
Auto <- na.omit(Auto) # Remove data rows including NA.
Auto$origin <- as.factor(Auto$origin) # Coerce the type of origin into factor


#a) 
res <- numeric(length = 392)
for (i in 1:392) {
  lmfit.loocv <- lm(mpg ~ horsepower, data = Auto[-i, ])
  yhat <- predict(lmfit.loocv, data.frame(horsepower = Auto$horsepower[i]))
  res[i] <- Auto[i,]$mpg - yhat
}
mean(res^2)

cvLine <- rep(0, 20)
for(i in 1:20)
{
  fitter  <- glm(mpg ~poly(horsepower,i), data = Auto)
  cvLine[i] <- cv.glm(Auto,fitter)$delta[1]
}
lmfit.loocv$coefficients
fitter$coefficients
cvLine
