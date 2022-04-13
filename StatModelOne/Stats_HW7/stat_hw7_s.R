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

accRange = range(Auto$acceleration)
accGrid = seq(from = accRange[1], to = accRange[2])

splineC3 = lm(horsepower ~ ns(acceleration, df=4), data=Auto)
pAccRange = predict(splineC3, newdata = list(acceleration = accGrid), se = T)

plot(acceleration, horsepower, col="black")
lines(accGrid, pAccRange$fit, lwd=2) 
lines(accGrid, pAccRange$fit+2*pAccRange$se, lty="dashed") 
lines(accGrid, pAccRange$fit-2*pAccRange$se, lty="dashed") 

#c 
plot(acceleration, horsepower, col="black")
smoothS = smooth.spline(acceleration, horsepower, cv = TRUE)
smoothS$df
lines(smoothS,col="blue",lwd=2)

#d 
plot(acceleration, horsepower, xlim=accRange, col="red")
LR2=loess(horsepower ~ acceleration, span=.2, data=Auto) 
LR5=loess(horsepower ~ acceleration, span=.5, data=Auto)

lines(accGrid, predict(LR2, data.frame(acceleration=accGrid)), col="green", lwd=2)
lines(accGrid, predict(LR5, data.frame(acceleration=accGrid)), col="blue", lwd=2)

LR2
LR5

#e and f
gam1 = lm(horsepower ~  ns(acceleration, 5), data = Auto)
gam2 = lm(horsepower ~ ns(weight) + ns(acceleration, 5), data = Auto)
gam3 = lm(horsepower ~ ns(weight, 4) + ns(acceleration, 5), data = Auto)
gam1
gam2
gam3
anova(gam1,gam2,gam3)



