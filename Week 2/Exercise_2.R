install.packages("readxl")
library("readxl")

install.packages("timeSeries")
library("timeSeries")

# EXERCISE 1
CBE <- read.table(file = "cbe.dat", header = TRUE)
View(CBE)
attach(CBE)


chocTS <- ts(choc, start = c(1958,1), end = c(1990,12), frequency = 12)
beerTS <- ts(beer, start = c(1958,1), end = c(1990,12), frequency = 12)
elecTS <- ts(elec, start = c(1958,1), end = c(1990,12), frequency = 12)

plot(cbind(chocTS, beerTS, elecTS), main = "Monthly Time Series for Chocolate, Beer, 
     Electricity")

## Part A
#*########################################
#remove seasonality
chocTS.withoutSeason <- aggregate(chocTS, FUN = mean)
beerTS.withoutSeason <- aggregate(beerTS, FUN = mean)
elecTS.withoutSeason <- aggregate(elecTS, FUN = mean)

plot(cbind(chocTS.withoutSeason, beerTS.withoutSeason, elecTS.withoutSeason), 
  main = "Annual Time Series for Chocolate, Beer, 
     Electricity")

## Part B
###########################################
#data from January 1958 to December 1988
chocTS.88 <- window(chocTS, start = c(1958,1), end = c(1988,12))
beerTS.88 <- window(beerTS, start = c(1958,1), end = c(1988,12))
elecTS.88 <- window(elecTS, start = c(1958,1), end = c(1988,12))

plot(cbind(chocTS.88, beerTS.88,elecTS.88), main = "Chocolate, Beer and Electricity Time Series 
     1958-1988")

## Part c
###########################################
#Estimate α and β 

#estimated time
choco.time <-  time(chocTS.88)
beer.time <- time(beerTS.88)
elec.time <- time(elecTS.88)

choco.lm <- lm(chocTS.88 ~choco.time)
beer.lm <- lm(beerTS.88 ~ beer.time)
elec.lm <- lm(elecTS.88 ~ elec.time)
elec.log.lm <- lm(log(elecTS.88) ~ elec.time)

## Summary
summary(choco.lm)  #  α = -246172.11 and β = 127.01
choco.lm$coefficients

summary(beer.lm)   #  α = -5499.2981 and β = 2.8556
beer.lm$coefficients

summary(elec.lm)   #  α = -653553.0508 and β = 334.1615 
elec.lm$coefficients

summary(elec.log.lm) #  α = -118.29298930 and β = 0.06426264
elec.log.lm$coefficients

## Part D
###########################################
#plot estimated values

plot(chocTS.88, main = "Monthly Chocolate Time Series")
abline(choco.lm, col = "red")

plot(beerTS.88, main = "Monthly Beer Time Series")
abline(beer.lm, col = "red")

plot(elecTS.88, main = "Monthly Electricity Time Series")
abline(elec.lm, col = "red")

plot(log(elecTS.88), main = "Monthly log(Electricity) Time Series")
abline(elec.log.lm, col = "red")

## Part E
###########################################
#Predict values for June 1989 and June 1990
# Prediction for chocolate in June 1989 and June 1990: 
-246172.11 + 127.01 * (1989 + 5/12)
-246172.11 + 127.01 * (1990 + 5/12)

# Prediction for beer in June 1989 and June 1990: 
5499.29814 + 2.85556 * (1989 + 5/12)
5499.29814 + 2.85556 * (1990 + 5/12)

# Prediction for electricity in June 1989 and June 1990: 
-653553.0508 + 334.1615 * (1989 + 5/12)
-653553.0508 + 334.1615 * (1990 + 5/12)

# Prediction for log(electricity) in June 1989 and June 1990:??
exp(-118.29298930 + 0.06426264 * (1989 + 5/12))
exp(-118.29298930 + 0.06426264 * (1990 + 5/12))


# EXERCISE 2
covid <- read_excel("cases of coronavirus.xlsx", sheet = 2)
View(covid)
attach(covid)

## pART A

covidInfections <- ts(Number, frequency = 7)#weekly, coz it doesnt fit a year

plot(covidInfections, ylab = "Number of new infections", xlab="Time in weeks",
     main = "Number of daily new infections of coronavirus 
     in Germany from January 28, 2020 to October 31, 2021")

# Part B
covidInfections.hw <- HoltWinters(covidInfections)

# estimates for the smoothing parameters
covidInfections.hw$alpha
covidInfections.hw$beta
covidInfections.hw$gamma

covidInfections.hw$coefficients


# Part C
plot(covidInfections.hw$fitted)

plot(covidInfections.hw)

# Part D
#make predictions for the next 4 weeks after October 31, 2021.
covidInfections.hw.prediction <- predict(covidInfections.hw, n.ahead = 4*7)
covidInfections.hw.prediction

plot(covidInfections.hw)
lines(covidInfections.hw.prediction, col = "red")

#PART E
#Apply the aggregate function to (xt) to get weekly data (yt). Use the double ex-
#ponential smoothing (i.e. Holt-Winters method without seasonal components) for the
#aggregated data (yt) to make predictions for the next 4 weeks after October 31, 2021.
covid.weekly <- aggregate(covidInfections, FUN = mean) #make weekly data

plot(covid.weekly, main = "Weekly Data")

covid.weekly.hw <- HoltWinters(covid.weekly, gamma = FALSE) #pass by HoltWinters

covid.weekly.hw.prediction <- predict(covid.weekly.hw, n.ahead = 4) #predict 4 weeks ahead

plot(covid.weekly.hw)
lines(covid.weekly.hw.prediction, col = "red")

#PART F
covidFull <- read_excel("cases of coronavirus Germany long.xlsx", sheet = 2)
View(covidFull)
covid_comb <- ts(c(Number, covidFull$NewCases[644:671]), frequency = 7)

plot(covid_comb)
lines(covidInfections.hw.prediction, col = "red")
