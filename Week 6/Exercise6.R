library(randtests)

#EXERCISE 1
set.seed(7)
X <- Z <- rnorm(1000)
#Z <- rnorm(1000, mean = 0, sd = sqrt(1))

for (t in 3:1000){
  X[t] <- (X[t-2]*0.8) + Z[t]
}

XTS <- ts(X)
## PART A
plot(XTS, main = expression("Timeseries "*X[t]))

acf(XTS)
pacf(XTS) #AR(2)


# Part B
xts.ar <- ar(XTS)
xts.ar$order
xts.ar

#PART C
xts.ar.residual <- xts.ar$resid

#TEST FOR independence
acf(na.omit(xts.ar.residual))
Box.test(xts.ar.residual, lag = 20, type = "Box-Pierce") #HO is not rejected
turning.point.test(na.omit(xts.ar.residual)) # HO is not rejected 
difference.sign.test(na.omit(xts.ar.residual)) # HO is not rejected

#test for gaussianity
qqnorm(xts.ar.residual)
qqline(xts.ar.residual, col = 2)

shapiro.test(xts.ar.residual) # HO is not rejected

# Exercise 3 Fitting an AR model
## Part A
cbe <- read.table(file = "cbe.dat", header = TRUE)
View(cbe)
attach(cbe)

#time series
elec <- ts(elec, start = c(1958, 1), end = c(1990,12), frequency = 12)

plot(elec, main = "Electricity (1958-1990)")

#decompose
elec.r <- log(decompose(elec, type = "multiplicative")$random)

plot(elec.r, main = "Random Component for Electricity")


## PART B

acf(na.omit(elec.r), main = "ACF Elec")
pacf(na.omit(elec.r), main = "PACF Elec")


## PART C
elec.r.ar <- ar(elec.r, na.action = na.omit) #AR Model
elec.r.resid <- na.omit(elec.r.ar$resid) #residuals

elec.r.ar$order #order
elec.r.ar$ar #coefficient

#test for independence 
acf(elec.r.resid)

Box.test(elec.r.resid, lag = 20) # HO does not reject the hypothesis
turning.point.test(elec.r.resid) # HO does not reject the hypothesis
difference.sign.test(elec.r.resid) # HO does not reject the hypothesis

# Test for Gaussianity 
qqnorm(elec.r.resid)
qqline(elec.r.resid, col = 2)

shapiro.test(elec.r.resid) # HO does not reject the hypothesis of Gaussianity

## PART D
elec.annual <- aggregate(elec, FUN = mean)
plot(elec.annual, main = "Annual Electricity")
acf(elec.annual)
pacf(elec.annual)

elec.annual.ar <- ar(elec.annual, method = "burg") #model
elec.annual.ar$order #order
elec.annual.ar$ar #coeffs

elec.annual.residual <- na.omit(elec.annual.ar$resid)

acf(elec.annual.residual)
Box.test(elec.annual.residual, type = "Ljung-Box", lag = 20) # HO does not reject the hypothesis
turning.point.test(elec.annual.residual) # HO does not reject the hypothesis
difference.sign.test(elec.annual.residual) # HO does not reject the hypothesis
