##### Chapter 7 Lab: Non-linear Modeling

library(ISLR)
Wage <- Wage
attach(Wage)

############# Regression Splines #############

library(splines)

#### Cubic spline fit #####

# Basis functions: instead of truncated power basis, we use B-spline basis.
# B-spline basis functions are equivalent bases for representing cubic splines.
# bs() generates the matrix of B-splines basis functions.
x <- seq(-1, 1, 0.01)
bsBasis <- bs(x, knots=c(-0.5, 0, 0.5)) 
# We can specify the degree of polynomial, but by default, cubic splines are produced
head(bsBasis)
# The degrees of freedom with 3 knots should be 3 + 4 = 7.
# The bs() function omits by default the constant term in the basis.
# and hence it produces 6 basis functions except the intercept term.
plot(x, bsBasis[,1], ylim = c(-1, 1), type='l')
abline(v=-1, lty=2)
abline(v=-0.5, lty=2)
abline(v=0, lty=2)
abline(v=0.5, lty=2)
abline(v=1, lty=2)
abline(h=0, lty=2)
lines(x, bsBasis[,2], type='l')
lines(x, bsBasis[,3], type='l')
lines(x, bsBasis[,4], type='l')
lines(x, bsBasis[,5], type='l')
lines(x, bsBasis[,6], type='l')


# Fit wage to age using a cubic spline
fit = lm(wage ~ bs(age, knots=c(25,40,60)), data = Wage)

# Create a grid of values of age at which we want predictions
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])

# Make prediction
pred = predict(fit, newdata = list(age=age.grid), se=T)

# Plotting
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2) # predicted wage by spline model
lines(age.grid, pred$fit+2*pred$se, lty="dashed") # upper confidence limit
lines(age.grid, pred$fit-2*pred$se, lty="dashed") # lower confidence limit

# Q) How many basis functions will be generated with knots at 25, 40, and 60?
dim(bs(age, knots=c(25,40,60)))
# Recall that a cubic spline with three knots has seven degrees of freedom;
# these 7 degrees of freedom are used up by an intercept, plus six basis functions

# We could also use the df option to produce a spline with knots at uniform quantiles of the data.
dim(bs(age, df=6)) # R chooses 25th, 50th, and 75th percentiles of age as knots.
# Chosen values of knots by R can be obtained by
attr(bs(age, df=6), "knots")




#### Natural cubic spline fit #####

# Basis functions
nsBasis <- ns(x, knots=c(-0.5, 0, 0.5))
head(nsBasis)
plot(x, nsBasis[,1], ylim = c(-0.8, 0.8), type='l')
abline(v=-1, lty=2)
abline(v=-0.5, lty=2)
abline(v=0, lty=2)
abline(v=0.5, lty=2)
abline(v=1, lty=2)
abline(h=0, lty=2)
lines(x, nsBasis[,2], type='l')
lines(x, nsBasis[,3], type='l')
lines(x, nsBasis[,4], type='l')


# Fit wage to age using a natural cubic spline
fit2 = lm(wage ~ ns(age, df=4), data=Wage) # natural spline with 4 dof
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)

plot(age, wage, col="gray")
lines(age.grid, pred2$fit, col="red", lwd=2)
lines(age.grid, pred2$fit+2*pred2$se, lty="dashed", col="red")
lines(age.grid, pred2$fit-2*pred2$se, lty="dashed", col="red")




############# Smoothing spline fit #############

plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")

fit = smooth.spline(age, wage, df=16)
# The function determines which value of lambda leads to 16 degrees of freedom.
fit$lambda

fit2 = smooth.spline(age, wage, cv=TRUE)
# The value of lambda is determined by CV. The corresponding dof is
fit2$df

lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


############# Local regression #############

plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")

fit=loess(wage ~ age, span=.2, data=Wage) # local linear regression using span 0.2
fit2=loess(wage ~ age, span=.5, data=Wage) # local linear regression using span 0.5

lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)



############# GAMs #############

# Predict wage using natural spline functions of year and age, 
# treating education as a qualitative predictor.
# We can do this using the lm() function.
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# If we want to use smoothing spline, instead of natural spline, we will need gam library
library(gam)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
# we use gam() function instead of lm().
# s(year, 4) : smoothing spline of year with 4 dof.
# s(age, 5) : smoothing spline of age with 5 dof.
# education will be converted into 4 dummy variables.

# Plotting
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
plot.Gam(gam1, se=TRUE, col="red")

# In these plots, the function of year looks rather linear.
# We can perform a series of ANOVA tests in order to determine which of these three models is best.
gam.m1 = gam(wage ~ s(age, 5) + education, data=Wage) # a GAM that excludes year
gam.m2 = gam(wage ~ year + s(age, 5) + education, data=Wage) # a GAM with a linear function of year
# gam.m3 is a GAM with a spline function of year

anova(gam.m1, gam.m2, gam.m3, test="F")
# gam.m2 is better than gam.m1, but there is no evidence that gam.m3 is better than gam.m2.
# summary(gam.m3)

# Make predictions
preds = predict(gam.m2, newdata = Wage) # make predictions on the training set

# We can also use local regression fits as building blocks in a GAM, using
# the lo() function.
gam.lo = gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education, data = Wage)
plot.Gam(gam.lo, se=TRUE, col="green")

# We can also use the lo() function to create interactions.
gam.lo.i = gam(wage ~ lo(year, age, span=0.5) + education, data = Wage)

# Plotting
library(akima)
plot(gam.lo.i)

# Logistic regression GAM
gam.lr = gam(I(wage>250) ~ year + s(age, df=5) + education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))
# we can see that there is no high earners in the < HS category.
# we re-fit with data set without this category. This provides more sensible results.
gam.lr.s = gam(I(wage>250) ~ year + s(age,df=5) + education, family=binomial, 
               data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

