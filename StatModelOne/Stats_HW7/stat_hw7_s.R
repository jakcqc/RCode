##Homework,7
library(splines)
library(ISLR)

#Problem,1
stepFR=t(matrix(c(
 3.5,0.2
 ,1.2,0.7
 ,6.9,1.2
 ,2.6,3.6
 ,4.4,3.8
 ,3.0,4.0
 ,8.5,4.2), nrow = 2))
colnames(stepFR) = c('x','y')
stepFR

means = c(0,0,0)
meanCount = c(0,0,0)
for(x in 1:6){
  if(stepFR[x,2] >= 3.7){
    means[3] = means[3] + stepFR[x,1]
    meanCount[3] = meanCount[3] + 1
  }
  if(stepFR[x,2] < 3.7 && stepFR[x,2] <= 1){
    means[2] = means[2] + stepFR[x,1]
    meanCount[2] = meanCount[2] + 1
    
  }
  if(stepFR[x,2] < 1){
    means[1] = means[1] + stepFR[x,1]
    meanCount[1] = meanCount[1] + 1
    
  }
}

means = means/meanCount
meansM = matrix(means, ncol = 3)
meansM
colnames(meansM) = c('B0', 'B1', 'B2')
meansM



#Problem 2
#see work on doc

#problem 3
Auto <- Auto
attach(Auto)


p1=lm(horsepower~acceleration,data=Auto)
p2=lm(horsepower~poly(acceleration,2),data=Auto)
p3=lm(horsepower~poly(acceleration,3),data=Auto)

anova(p1,p2,p3)

#b 


