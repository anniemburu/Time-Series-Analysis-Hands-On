## Exercise 1

## (a)

library(randtests)
set.seed(2)
x <- z <- rnorm(500)
for (t in 2:500) {
  x[t] <- x[t-1]+ z[t] + 0.6*z[t-1]
}


xTS <- ts(x)
plot(xTS, main = expression("Time series "*(x[t])))

# acf and pacf plots do not make sense, since (x_t) is not stationary

acf(xTS, main = expression("Sample autocorrelation function of "*(x[t])))
pacf(xTS, 
     main = expression("Sample partial autocorrelation function of "*(x[t])))


## (c)

# fitting an AR-model does not work correctly, since (x_t) isn't stationary
xTS_ar <- ar(xTS)
xTS_ar$order
xTS_ar$ar
xTS_ar
acf(xTS_ar$resid, na.action = na.pass)

# differencing of (x_t)

yTS <- diff(xTS)
plot(yTS, main = expression("Time series "*(y[t])))
acf(yTS, main = expression("Sample autocorrelation function of "*(y[t])))
pacf(yTS, 
     main = expression("Sample partial autocorrelation function of "*(y[t])))


## (d)

# fitting an MA(1)-model to (y_t)

yTS_ma <- arima(yTS, order = c(0,0,1))
yTS_r <- yTS_ma$residuals
acf(na.omit(yTS_r), main = "ACF for residuals of y_t")


# or fitting an ARIMA(0,1,1)-model to (x_t)

xTS_arima <- arima(xTS, order = c(0,1,1))
xTS_r <- ts(xTS_arima$residuals)
acf(na.omit(xTS_r), main = "ACF for residuals of x_t")

# checking independence/uncorrelatednes

Box.test(na.omit(xTS_r), lag = 20, type = "Box-Pierce")     # H0 is not rejected
turning.point.test(na.omit(xTS_r))                          # H0 is not rejected
difference.sign.test(na.omit(xTS_r))                        # H0 is not rejected

## No significant evidence against the null hypothesis that the residuals are iid. 
## Hence, we may perform tests for Gaussianity.

# checking Gaussianity

qqnorm(xTS_r)
shapiro.test(xTS_r)             # H0 is not rejected

## No significant evidence against the null hypothesis of Gaussianity.


#################################################
#################################################
## Exercise 2

polyroot(c(1,-1.3,0.3))
polyroot(c(1,-1.2,0.36))
polyroot(c(1,-0.4,-0.6))


##############################################
#################################################
## Exercise 3

## (a) 

library(tseries)
CBE <- read.table(file = "cbe.dat", header=TRUE)
attach(CBE)

elecTS <- ts(elec, start = c(1958,1), 
             end = c(1990,12), frequency = 12)
plot(elecTS, main = "Monthly Time Series for Electricity")


elecTS_log <- log(elecTS)

## (b)

plot(elecTS_log, main = "Monthly Time Series for log(Electricity)")
elecTS_log_D1 <- diff(elecTS_log, lag = 12)
plot(elecTS_log_D1, main = "Differenced Time Series for log(Electricity)")

## (c)

kpss.test(elecTS_log_D1)          # H0 (= process is stationary) is rejected

## (d)

elecTS_log_D1D2 <- diff(elecTS_log_D1)
plot(elecTS_log_D1D2)

kpss.test(elecTS_log_D1D2)   # H0 (= process is stationary) is not rejected
adf.test(elecTS_log_D1D2)    # H0 (= process is not stationary) is rejected


## (e)

arima(elecTS_log_D1D2, order = c(1,0,0))$aic
arima(elecTS_log_D1D2, order = c(0,0,1))$aic
arima(elecTS_log_D1D2, order = c(1,0,1))$aic
arima(elecTS_log_D1D2, order = c(2,0,0))$aic
arima(elecTS_log_D1D2, order = c(0,0,2))$aic
arima(elecTS_log_D1D2, order = c(2,0,1))$aic
arima(elecTS_log_D1D2, order = c(1,0,2))$aic
arima(elecTS_log_D1D2, order = c(2,0,2))$aic

# The smallest AIC = -1768.133 for ARIMA(1,0,2).

acf(arima(elecTS_log_D1D2, order = c(1,0,2))$residuals, na.action = na.pass)

Box.test(na.omit(arima(elecTS_log_D1D2, order = c(1,0,2))$residuals), 
         lag = 24, type = "Box-Pierce")                 # H0 is rejected
turning.point.test(na.omit(arima(elecTS_log_D1D2, 
                                 order = c(1,0,2))$residuals))         # H0 is not rejected
difference.sign.test(na.omit(arima(elecTS_log_D1D2, 
                                   order = c(1,0,2))$residuals))         # H0 is not rejected
## Significant evidence against the null hypothesis that the residuals iid. 


## (f)

for(p in 0:1){
  for(q in 0:1){
    for (P in 0:1){
      for (Q in 0:1){
        print(c(p,1,q,P,1,Q))
        print(AIC(arima(elecTS_log, order = c(p,1,q), 
                        seasonal = list(order = c(P, 1, Q), period = 12))))
      } 
    }
  }
}

# The smallest AIC = -1873.532 for ARIMA(0,1,1)(1,1,1)_12.

elecTS_log_sarima_r <- arima(elecTS_log, order = c(0,1,1), 
                             seasonal = list(order = c(1, 1, 1), period = 12))$residuals

acf(elecTS_log_sarima_r, na.action = na.pass)

Box.test(na.omit(elecTS_log_sarima_r), 
         lag = 24, type = "Box-Pierce")                 # H0 is rejected
turning.point.test(na.omit(elecTS_log_sarima_r))        # H0 is not rejected
difference.sign.test(na.omit(elecTS_log_sarima_r))      # H0 is not rejected
