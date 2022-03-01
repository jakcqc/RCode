## homework 4
library(MASS)

##Problem 1 
#
#
#
##Problem 2
#
#a) If odds are .12, then .12 = prob fruad / 1- prob fruad
#.12 - .12x = x
#.12 = 1.12x
#.12/1.12 = x
#therfore, prob of fraud is = .1071429
#b) If prob of increase is .52, then .52 / (1 - .52)  = odds of 1.083333333
#
#
#Problem 3
Auto <- read.csv("Auto.csv", na.strings ="?") # With the option, R recognizes ? as NA.
Auto <- na.omit(Auto) # Remove data rows including NA.
Auto.class <- Auto[,c(1,2,4,6,7,8)] # Keep only mpg, cylinders, horsepower,
# acceleration, year, and origin
Auto.class <- Auto.class[Auto.class$origin != '2',] # Only keep data points where
# origin is either 1 (American) or 3 (Japanese).
Auto.class$origin <- as.factor(Auto.class$origin) # Coerce the type of origin into factor
#a and b) 
summary(Auto.class)
newLogRR = glm(Auto.class$origin ~ ., data = Auto.class, family = binomial)
summary(newLogRR)
#c)
makeConProb = predict(newLogRR, type='response')
makeConPred =rep("American", length(makeConProb))
makeConPred[makeConProb > 0.5] = "Japanese"
newOT = table(makeConPred, Auto.class$origin)
newOT
(newOT[1] + newOT[4]) / (newOT[1]+newOT[2]+ newOT[3]+newOT[4])

#d 
autoLda = lda(Auto.class$origin ~., data = Auto.class)
autoLda
#e
plot(autoLda)
#f
autoLdaPred = predict(autoLda, newdata = Auto.class , type = "response")
autoLdaPredT = table(autoLdaPred$class, Auto.class$origin)
autoLdaPredT
#g 
autoQda = qda(Auto.class$origin ~., data = Auto.class)
autoQda
#h
autoQdaPred = predict(autoQda, newdata = Auto.class , type = "response")
autoQdaPredT = table(autoQdaPred$class, Auto.class$origin)
autoQdaPredT
#i
summerT <- function(table)
{
  tempV = 0
  for(i in table)
  {
    tempV = i + tempV
  }
  return(tempV)
}
#accuracy
(autoLdaPredT[1] + autoLdaPredT[4]) / summerT(autoLdaPredT)
(autoQdaPredT[1] + autoQdaPredT[4]) / summerT(autoQdaPredT)
#sens
autoLdaPredT[4] / (autoLdaPredT[2] + autoLdaPredT[4])
autoQdaPredT[4] / (autoQdaPredT[2] + autoQdaPredT[4])
#specif 
autoLdaPredT[1] / (autoLdaPredT[3] + autoLdaPredT[1])
autoQdaPredT[1] / (autoQdaPredT[3] + autoQdaPredT[1])
#precision 
autoLdaPredT[4] / (autoLdaPredT[4] + autoLdaPredT[3])
autoQdaPredT[4] / (autoQdaPredT[4] + autoQdaPredT[3])

