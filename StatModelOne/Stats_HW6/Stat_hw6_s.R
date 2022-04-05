library(leaps)
library(ISLR)
library(glmnet)
library(pls)
set.seed(1)
x1 <- runif(100, -1.7, 1.7)
x2 <- x1^2; x3 <- x1^3; x4 <- x1^4; x5 <- x1^5
x6 <- x1^6; x7 <- x1^7; x8 <- x1^8; x9 <- x1^9
x10 <- x1^10
y <- -1.3 + 2*x1 + 1.5*x2 - 2*x3 + rnorm(100)
data_df <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
data_mat <- as.matrix(data_df)
# We prepare the data set in two different objects: data_df (data frame), data_mat (matrix)
plot(x1,y)

?regsubsets
sFit = regsubsets(y ~ ., data = data_df)
mainSumSub = summary(sFit)

plot(mainSumSub$cp)
plot(mainSumSub$bic)
plot(mainSumSub$adjr2)

coef(sFit, 4)
coef(sFit, 3)
coef(sFit, 6)

sFitBack = regsubsets(y ~ ., data = data_df, method = "backward")
backSum = summary(sFitBack)
plot(backSum$cp)
coef(sFitBack, 4)

sFitFor = regsubsets(y ~ ., data = data_df, method = "forward")
forSum = summary(sFitFor)
plot(forSum$bic)
coef(sFitFor, 4)


#problem 2
grid=10^seq(10,-2,length=100)

x = model.matrix(y~.,data_df)[,-1]
newY = data_df[1]

set.seed(1)
trainD = sample(1:nrow(x), nrow(x)*.7)
testD = (-trainD)
y.test = y[testD]
lasso.mod = glmnet(x[trainD,], y[trainD], alpha=1)
plot(lasso.mod)

set.seed(2)

crossV = cv.glmnet(x[trainD,], y[trainD], alpha=1)
plot(crossV)
bestL = crossV$lambda.min
bestL

lasso.pred = predict(lasso.mod, s = bestL, newx = x[testD,])
mean((lasso.pred-y.test)^2)

fullFit = glmnet(x,y,alpha=1, lambda=grid)
lCoef = predict(fullFit, type = "coefficients", s=bestL)[1:20]
lCoef[lCoef!=0]


set.seed(3)
pFit = pcr(y~., data = data_df, subset=trainD, scale = TRUE, validation="CV")
summary(pFit)
validationplot(pFit,val.type="MSEP")

pPred = predict(pFit, x[testD,], ncomp=6)
mean((pPred-y.test)^2)

pFitFull =pcr(y~., data=data_df, scale=TRUE,ncomp=6)
summary(pFitFull)

