library(fracdiff)
library(tseries)
library(randtests)

# Exercise 1 Simulation of a FARIMA process
#simulate

farima <- function(kp,diff,kq){
  set.seed(5)
  ts(fracdiff.sim(500, ar = kp, ma = kq, d = diff )$series) 
}

plot(farima(0.1, 0.25,-0.1), main = "FARIMA with d = 0.25")
plot(farima(0.1, 0.45,-0.1), main = "FARIMA with d = 0.45")



# Exercise 2 Fitting a FARIMA model