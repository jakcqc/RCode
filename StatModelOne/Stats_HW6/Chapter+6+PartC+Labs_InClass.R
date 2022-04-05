######### Chapter 6 Lab 3: PCR and PLS Regression

library(ISLR)
Hitters <- Hitters
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

### Principal Components Regression

library(pls)

## Perform PCR on the training data and evaluate its test set performance.
# Split data into training and test sets
set.seed(2)
train=sample(1:nrow(Hitters), nrow(Hitters)/2)
test=(-train)
y.test=Hitters$Salary[test]

# Perform model training
set.seed(3)
pcr.fit= pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
# By the option, subset=train, we only use training set to perform 10-fold CV
# Setting scale=TRUE has the effect of standardizing each predictor
# so that the scale of each variable will not have an effect.
# validation="CV" makes pcr() perform the 10-fold CV for each possible 
# value of M, the number of principal components used.
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

# Model testing
pcr.pred=predict(pcr.fit, Hitters[test,], ncomp=6)
mean((pcr.pred-y.test)^2)

# Finally, we fit PCR on the full data set, 
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE,ncomp=6)
summary(pcr.fit)



### Partial Least Squares

# Perform model training
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
# The lowest CV error occurs when only M = 2 partial least squares directions are used.

# Model testing
pls.pred=predict(pls.fit,Hitters[test,],ncomp=2)
mean((pls.pred-y.test)^2)

# Final model fit using the full data set
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)



