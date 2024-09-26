library(randtests)
#EXERCISE 1
## Part A
X <- rnorm(200, mean = 0, sd = sqrt(4))
XTS <- ts(X)
plot(XTS)
abline(a=0, b=0, col = 2)
acf(XTS)  # no sig correlation

## Part B
Box.test(XTS, type="Box-Pierce") # H0 is not rejected
turning.point.test(XTS) #HO is not rejected
difference.sign.test(XTS) #HO is not rejected

## PART C
qqnorm(XTS)
qqline(XTS, col=2)

#PART D
shapiro.test(XTS)  # no significant evidence againts the  null hypothesis of Gaussianity

#EXERCISE 3

## PART A
global <- scan(file = "global.dat")
View(global)
#attach(global)

globalTS <- ts(global, start = c(1856,1), end = c(2005,12), frequency = 12)
plot(globalTS)

#PART B
global.annual <- aggregate(globalTS, FUN = mean)
plot(global.annual)

#PART C
ar.model <-  ar(global.annual)
ar.model$order
ar.model$ar

#PART D
ar.model.residual <- ar.model$resid
length(ar.model.residual)

acf(na.omit(ar.model.residual), main = "ACF of the residula of the AR Model")
## resid had no dependence

Box.test(ar.model.residual, type = "Box-Pierce", lag = 20) #test for independence, H0 is not rejected
turning.point.test(na.omit(ar.model.residual)) # HO is not rejected
difference.sign.test(na.omit(ar.model.residual)) # HO is not rejected

## No significant evidence against the null hypothesis that the residuals are iid. 
## Hence, we may perform tests for Gaussianity.

qqnorm(ar.model.residual)
qqline(ar.model.residual, col = 2)
shapiro.test(ar.model.residual) # p-value = 0.6348



