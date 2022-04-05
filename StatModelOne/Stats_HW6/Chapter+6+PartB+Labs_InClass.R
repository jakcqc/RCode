######### Chapter 6 Lab 2: Ridge Regression and the Lasso

library(ISLR)
Hitters <- Hitters
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

library(glmnet)
# We will use the glmnet package in order to perform ridge regression and the lasso.

x=model.matrix(Salary~.,Hitters)[,-1]
# glmnet() can only take numerical, quantitative inputs.
# When we have a qualitative variable, we have to transform it into dummy variables.
# A convenient way to do this is to use model.matrix() function.

y=Hitters$Salary


##### Ridge Regression

# Prepare a grid of lambda values
grid=10^seq(10,-2,length=100)
# Model fit
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # alpha=0: ridge, alpha=1: lasso
# Coefficient estimates outcome
dim(coef(ridge.mod)) # row: coefficients, column: lambda values
# Check 50th lambda value
ridge.mod$lambda[50]
# Check corresponding coefficient estimates
coef(ridge.mod)[,50]
# ell-2 norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))
# Coefficient estimates with a new lambda value (s)
predict(ridge.mod,s=100,type="coefficients")[1:20,]

#### Model selection (find best lambda)
## Split data into training and test sets
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
# train the model using training set

ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) 
# Given the value of lambda, we evaluate the model using test set.
mean((ridge.pred-y.test)^2) # test MSE

## Cross validation to determine best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) # 10-fold CV by default
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
# evaluate the model with bestlam for the test set
mean((ridge.pred-y.test)^2) # test MSE with bestlam

## Finally, we refit our ridge regression model on the full data set, 
## using the value of lambda chosen by cross-validation
out=glmnet(x,y,alpha=0)
# obtain the coefficient estimates
predict(out,type="coefficients",s=bestlam)[1:20,]




##### The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

## CV to find best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

## Evaluate the model with bestlam by test set
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

## Refit the model on the full data set with bestlam
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
