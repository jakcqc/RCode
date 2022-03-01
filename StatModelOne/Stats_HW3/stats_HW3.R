library (MASS)
library (ISLR2)

##problem 1
head(Boston)

lm.fit <- lm(medv ~ lstat , data = Boston)
attach (Boston)
lm.fit <- lm(medv ~ lstat)
summary (lm.fit)
names (lm.fit)
confint (lm.fit)
predict (lm.fit , data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict (lm.fit , data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
plot (lstat , medv)
abline (lm.fit)

abline (lm.fit , lwd = 3)
abline (lm.fit , lwd = 3, col = " red ")
plot (lstat , medv , col = " red ")
plot (lstat , medv , pch = 20)
plot (lstat , medv , pch = "+")
plot (1:20, 1:20, pch = 1:20)
par (mfrow = c(2, 2))
plot (lm.fit)

plot ( predict (lm.fit), residuals (lm.fit))
plot ( predict (lm.fit), rstudent (lm.fit))
plot ( hatvalues (lm.fit))
which.max ( hatvalues (lm.fit))

lm.fit <- lm(medv ~ lstat + age , data = Boston)
summary (lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)
summary (lm.fit)
library(car)
vif(lm.fit)
lm.fit1 <- lm(medv ~ . - age , data = Boston)
summary (lm.fit1)
lm.fit1 <- update (lm.fit , ~ . - age)
summary (lm(medv ~ lstat * age , data = Boston))
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary (lm.fit2)
lm.fit <- lm(medv ~ lstat)
anova (lm.fit , lm.fit2)
par (mfrow = c(2, 2))
plot (lm.fit2)

lm.fit5 <- lm(medv ~ poly (lstat , 5))
summary (lm.fit5)
summary (lm(medv ~ log(rm), data = Boston))
head (Carseats)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age , data = Carseats)
summary (lm.fit)
attach (Carseats)
contrasts (ShelveLoc)


#problem 2
Auto <- read.csv("Auto.csv", na.strings ="?") # With the option, R recognizes ? as NA.
Auto <- na.omit(Auto) # Remove data rows including NA.
Auto$origin <- as.factor(Auto$origin) # Coerce the type of origin into factor
pairs(Auto[,1:8])
count <- 1
countTwo <- 2
while(count > 0){
  while(countTwo > 0){
    
    print(cor(Auto[count], Auto[countTwo]))
    countTwo = countTwo + 1
    if(countTwo > 7)
    {
      countTwo = 0
    }
  }
  count = count + 1
  countTwo = count + 1
  if(count > 6)
  {
    count = 0
  }
}

lm.fitMPG <- lm(Auto$mpg ~ . - name - displacement - weight, data = Auto)
plot(lm.fitMPG)
summary(lm.fitMPG)

contrasts(Auto$origin)
?Contrasts

lm.fitMPGC  <- lm(Auto$mpg ~ Auto$cylinders, data = Auto)
plot(lm.fitMPGC)
summary(lm.fitMPGC)

?contrasts
Auto$origin
contrasts(t(Auto$origin))

vif(lm.fitMPG)

plot(lm.fitMPG)

lm.fitHOInt <- lm(Auto$mpg ~ . - name + (horsepower:origin), data = Auto)
summary(lm.fitHOInt)

lm.fitHOInt <- lm(Auto$mpg ~ . - name - displacement - weight + (horsepower:origin), data = Auto)
summary(lm.fitHOInt)
