##### Chapter 7 Lab: Non-linear Modeling

library(ISLR)
Wage <- Wage
attach(Wage)
plot(age,wage,cex=.5,col="darkgrey")

############# Polynomial Linear Regression ################

# Basis function for orthogonal polynomial
x <- seq(-1, 1, 0.01)
orthoPolyBasis <- poly(x,4) # fourth-degree orthogonal polynomial
plot(x, orthoPolyBasis[,1], type='l')
abline(h=0, lty=2)
abline(v=0, lty=2)
lines(x, orthoPolyBasis[,2], type='l')
lines(x, orthoPolyBasis[,3], type='l')
lines(x, orthoPolyBasis[,4], type='l')

# Basis function for (non-orthogonal) polynomial
x <- seq(-1, 1, 0.01)
PolyBasis <- poly(x,4, raw=T) # fourth-degree (non-orthogonal) polynomial
plot(x, PolyBasis[,1], type='l')
abline(h=0, lty=2)
abline(v=0, lty=2)
lines(x, PolyBasis[,2], type='l')
lines(x, PolyBasis[,3], type='l')
lines(x, PolyBasis[,4], type='l')

# We now apply orthogonal polynomial model to age from Wage data
fit=lm(wage~poly(age,4),data=Wage) # fourth-degree orthogonal polynomial
coef(summary(fit))

# We can also use polynomial directly without orthogonalization.
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
# Parameter estimates are different between orthogonal poly and poly, but
# it turns out there is no difference in terms of fitted values.

# Other equivalent ways
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)

# Create a grid of values of age at which we want predictions
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])

# Make predictions
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot data and fit
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0)) # control margins of plot
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# One way to decide the degree of polynomial is to use hypothesis tests.
# We use anova function to do this.
# Null hypothesis: A model 1 (degree d) is sufficient to explain the data
# Alternative hypothesis: A model 2 (degree d+1) is required to explain the data
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# Either a cubic or a quartic polynomial appear to provide a reasonable fit to the data

# We can extract the same information from the p-values associated with 
# each orthogonal polynomial basis function.
coef(summary(fit.5))

# We can also use anova() to compare following three models:
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

# In the following case (non-nested structure), we can't use anova() for model comparison
fit.edu <- lm(wage ~ education, data = Wage)
fit.age <- lm(wage ~ age, data = Wage)



############# Polynomial Logistic Regression ################
# Now we consider a classfication problem where we want to predict whether
# an individual earns more than $250,000 per year.

fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
# The expression wage>250 produces a logical variable containing TRUEs and FALSEs,
# which glm() coerces to binary by setting the TRUEs to 1 and the FALSEs to 0.

preds=predict(fit,newdata=list(age=age.grid),se=T)
# The default prediction type for a glm() model is type="link".
# This means we get predictions for the logit (log of odds).
# In order to obtain confidence intervals for Pr(Y=1|X) we use the transformation
pfit=exp(preds$fit)/(1+exp(preds$fit))

# The standard errors are also of logit form. 
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
# We also apply transformation to the std. error.
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# Plot data and fit
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

############# Step Function (Piecewise Constant Regression) ################
table(cut(age,4))
# cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age.

# Model fit
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

# Make predictions
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot data and fit
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Piecewise Constant",outer=T)
lines(age.grid,preds$fit,lwd=2,col="darkgreen")
matlines(age.grid,se.bands,lwd=1,col="darkgreen",lty=3)


########## Step Function (Piecewise Constant Logistic Regression) #############
# Model fit
fit=glm(I(wage>250)~cut(age,4),data=Wage,family=binomial)

# Prediction
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# Plot data and fit
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="darkgreen")
matlines(age.grid,se.bands,lwd=1,col="darkgreen",lty=3)



