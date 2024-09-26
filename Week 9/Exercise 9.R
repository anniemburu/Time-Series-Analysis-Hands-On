library(tseries)
library(randtests)

# Exercise 1 Simulation of an ARCH process
set.seed(5)
x <- z <- rnorm(500)

for (t in 2:500) {
  x[t] <- z[t] * sqrt(0.01 + 0.9*x[t-1]^2)
}

#PART A
xts <- ts(x)
plot(xts, main = expression(" Simulated Time Series "*X[t]))
abline(a = 0, b = 0, col = 2)

#stationarity ? YES
kpss.test(xts)
adf.test(xts)

# correlatedness ? Yes not correlated
acf(xts, main = expression("Sample ACF for "*X[t]))
Box.test(xts, lag=20, type = "Box-Pierce")

#independence? YES
difference.sign.test(xts)
turning.point.test(xts)

#PART B
acf(xts, main = expression("Sample ACF for "*X[t]))
acf(xts^2, main = expression("Sample ACF for "*X[t]^2)) #doesnt hold here
# X_t is nit iid noise

#PART C
# Arch/Garch-model can be suitable. How to choose the order?

acf(xts^2, main = expression(" Sample ACF for "*X[t]^2))
pacf(xts^2, main = expression(" Sample PACF for "*X[t]^2))

#PART D

xts.arch <- garch(xts, order = c(0,1), trace = FALSE)
xts.arch.resid <- xts.arch$residuals
plot(xts.arch.resid, main = expression("Residual for GARCH model on "*X[t]))

#test on Xt
acf(na.omit(xts.arch.resid))
Box.test(na.omit(xts.arch.resid), type = "Box-Pierce" ,lag = 20)

#test on XT^2
acf(na.omit(xts.arch.resid)^2)
Box.test(na.omit(xts.arch.resid)^2, type = "Box-Pierce", lag = 20)

#Gaussianity
qqnorm(xts.arch.resid)
qqline(xts.arch.resid, col =2)


#PART E
summary(xts.arch) # 0.012 and 0.7193 ~ almost similar to 0.1 and 0.8

# Exercise 2 Fitting a GARCH model
google <- read.table(file = "google.dat", header = TRUE)
View(google)

#PART A
google.xts <- ts(google, frequency = 7)
plot(google.xts, main = "Goolge daily stocks")
abline(a= 0.0027, b= 0, col=2)

#stationarity
kpss.test(google.xts)
adf.test(google.xts)

#correlation
acf(google.xts, main = expression("Sample ACF for google data"))
Box.test(google.xts, lag = 20, type = "Box-Pierce")

#independence
turning.point.test(google.xts)
difference.sign.test(google.xts)

#PART B
#check iid

acf(google.xts, main = expression("Sample ACF for google data"))
acf(google.xts^2, main = expression("Sample ACF for google data squared"))
#not iid

#PART C
for (p in 0:1){
  for (q in 0:1){
    if (p == 0 & q ==0){
      next
    } else {
      aic_vals <- AIC(garch(google.xts, order = c(q,p), trace = FALSE))
      #cat(" Order : ", c(q,p), "; AIC : ", aic_vals, "\n")
      print(c(p,q))
      print(aic_vals)
    }
  }
}

#model
google.xts.garch <- garch(google.xts, order = c(1,1), trace = FALSE)
google.xts.garch.resid <- google.xts.garch$residuals

acf(na.omit(google.xts.garch.resid))
Box.test(na.omit(google.xts.garch.resid), lag = 20, type = "Box-Pierce")

acf(na.omit(google.xts.garch.resid)^2)
Box.test(na.omit(google.xts.garch.resid), lag = 20, type = "Box-Pierce")

#independece
difference.sign.test(google.xts.garch.resid)
turning.point.test(na.omit(google.xts.garch.resid))

#Gaussianity
qqnorm(google.xts.garch.resid)
qqline(google.xts.garch.resid, col =2)
shapiro.test(google.xts.garch.resid)

# Exercise 3 Fitting a GARCH model

#PART A
#PART B
#PART C

#PART D