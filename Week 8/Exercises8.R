# Exercise 1 Simulation of an ARIMA process
library(randtests)

set.seed(2)
X <- Z <- rnorm(500)

for (t in 2:500) {
  X[t] <- X[t-1] + Z[t] + 0.6*Z[t-1]
}

#PART A
xts <- ts(X)

plot(xts, main = expression(" Simulated Timeseries "*X[t]))  #not stationary

# PART B
# acf and pacf dont make sense coz the ts is not stationary
acf(xts)
pacf(xts)

#PART C
yts <- ts(diff(xts))
plot(yts, main = expression(" Simulated Timeseries "*Y[t])) #stationary

acf(yts)
pacf(yts)

#X can be modeled by ARIMA
# Y can be modelled by MA(1)

# PART D
##fit MA(1)

yts.ma <- arima(yts, order = c(0,0,1))
yts.ma.resid <- yts.ma$residuals
acf(yts.ma.resid, main = expression("ACF for residuals of "*Y[t]))

#independence & correlatedness
Box.test(yts.ma.resid, lag = 20) # H0 is not rejected
turning.point.test(yts.ma.resid) # H0 is not rejected
difference.sign.test(yts.ma.resid) # H0 is not rejected

#Gaussianity
qqnorm(yts.ma.resid)
qqline(yts.ma.resid, col = 2)
shapiro.test(yts.ma.resid) # H0 is not rejected

##fit ARIMA
acf(xts)
pacf(xts)

for (p in 0:2){
  for (q in 0:2){
    value <- arima(xts, order = c(p,1,q))$aic
    cat("Order : ", c(p,1,q), ": AIC : ", value, "\n")
  }
}


xts.arima <- arima(xts, order = c(0,1,1))
xts.arima.resid <- xts.arima$residuals
acf(xts.arima.resid)

#independence & correlatedness
Box.test(xts.arima.resid, lag = 20) # H0 is not rejected
turning.point.test(xts.arima.resid) # H0 is not rejected
difference.sign.test(xts.arima.resid) # H0 is not rejected

#Gaussianity
qqnorm(xts.arima.resid)
qqline(xts.arima.resid, col = 2)
shapiro.test(xts.arima.resid) # H0 is not rejected

# Exercise 3 Fitting (S)ARIMA models
cbe <- read.table(file = "cbe.dat", header = TRUE)
#View(cbe)
attach(cbe)

# EXERCISE 3

# PART A
elec <- ts(elec, start = c(1958,1), end = c(1990,12), frequency = 12)
plot(elec, main = "Monthly Time Series Electricity")

elec.log = log(elec)

# PART B
plot(elec.log, main = "Monthly Time Series log(Electricity)")

#remove seasonality
elec.log.d1 <- diff(elec.log, lag = 12) 

plot(elec.log.d1, main = " Differenced log(Electricity)")

#test for stationarity
#kpss.test(elec.log.d1)
#adf.test(elec.log.d1)

# PART C
kpss.test(elec.log.d1)


# PART D
elec.log.d2 <- diff(elec.log.d1)

plot(elec.log.d2, main = "Twice Differenced log(Electricity)")
kpss.test(elec.log.d2)
adf.test(elec.log.d2)

# PART E
for (p in 0:2){
  for (q in 0:2){
    val <- arima(elec.log.d2, order = c(p,0,q))$aic
    cat("Order : ", c(p,0,q), "; AIC : ", val , "\n")
  }
}

#model
elec.log.d2.arma <- arima(elec.log.d2, order = c(1,0,2))
elec.log.d2.arma.resid <- elec.log.d2.arma$residuals

acf(elec.log.d2.arma.resid)
Box.test(elec.log.d2.arma.resid, lag = 20) # H0 is rejected
turning.point.test(elec.log.d2.arma.resid) # H0 is not rejected
difference.sign.test(elec.log.d2.arma.resid) # H0 is not rejected

#bad results coz model is wrong

# PART F :  ARIMA(p, 1, q)(P, 1, Q)_12
for(p in 0:1){
  for (q in 0:1){
    for (P in 0:1){
      for (Q in 0:1){
        value <- AIC(arima(elec.log, order = c(p, 1, q),
                      seasonal = list(order = c(P, 1, Q), period = 12)))
        cat("Order : ", c(p,1,q,P,1,Q), "; AIC : ", value , "\n")
        
      }
    }
  }
}

#model
elec.log.sarima <- arima(elec.log, order = c(0, 1, 1),
                         seasonal = list(order = c(1, 1, 1), period = 12))
elec.log.sarima.residual <- elec.log.sarima$residuals

acf(elec.log.sarima.residual)
Box.test(elec.log.sarima.residual, lag = 20) # H0 is not rejected
turning.point.test(elec.log.sarima.residual) # H0 is not rejected
difference.sign.test(elec.log.sarima.residual) # H0 is not rejected
