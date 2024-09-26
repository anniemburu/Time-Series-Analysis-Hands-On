#EXERCISE 1 : Confidence interval and testing the independence of a time series
#library(forecast)
library(randtests)

set.seed(5)
x <- rnorm(101, mean = 0, sd = sqrt(3))
#Part A

XTS <- ts(x[1:100])
XTS

plot(XTS, main = "Simulated Time Series")
abline(a=0, b=0, col=2)

#Part B
mean(XTS) # 0.05479346
var(XTS)  # 2.680688

#PART C
# lower confidence bound
mean(XTS)-qt(0.975, 99)*sqrt(var(XTS))/10    # Output: -0.2700785

# upper confidence bound  
mean(XTS)+qt(0.975, 99)*sqrt(var(XTS))/10 #  Output:


## Part D
acf(XTS)  #no correlation

## PART E

Box.test(XTS, lag = 20, type = "Box-Pierce")  # H0 is not rejected
turning.point.test(XTS)  # 
difference.sign.test(XTS)  # H0 is not rejected

#   EXERCISE 2
souv <- read.table(file ="sales-souvenirs.dat", header = FALSE)
View(souv)
attach(souv)

## Part A

souvTS <- ts(V1, start = c(1987,1), end = c(1993,12), frequency = 12)

plot(souvTS)

souvR <- decompose(souvTS, type = "multiplicative")$random
plot(souvR)

##Part B
acf(na.omit(souvR))

## PART C
length(souvTS)

Box.test(souvR, type = "Ljung-Box", lag = 10) #HO is rejected
turning.point.test(na.omit(souvR)) #HO is not rejected
difference.sign.test(na.omit(souvR)) #HO is not rejected
