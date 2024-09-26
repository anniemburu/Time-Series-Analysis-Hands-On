#install.packages("readxl")
library("readxl")

install.packages("timeSeries")
library("timeSeries")

## 1. Import the dataset into R.
CBE <- read.table(file = "cbe.dat", header = TRUE)
View(CBE)
attach(CBE)

## Part B: Create the variables into time series
chocTS <- ts(choc, start = c(1958,1), end = c(1990,12), frequency = 12)
beerTS <- ts(beer, start = c(1958,1), end = c(1990,12), frequency = 12)
elecTS <- ts(elec, start = c(1958,1), end = c(1990,12), frequency = 12)

plot(cbind(chocTS, beerTS, elecTS), main = "Time Series for Chocolate, Beer, 
     Electricity")

## Part C : Decompose the three time series
plot(decompose(chocTS, type = "multiplicative"))

plot(decompose(beerTS, type = "multiplicative"))

plot(decompose(elecTS, type = "multiplicative"))

#Exercise 2 Removal of Seasonal Effects

#PART A
globalTemp <- scan('global.dat')

globalTempTS <- ts(globalTemp, start = c(1856,1), end = c(2005,12), frequency = 12)

plot(globalTempTS, main = " Global Temperatures")

#Part B
yearly <- aggregate(globalTempTS, FUN = "mean")

plot(yearly, main =  "Global Yearly Temperatures", ylab = "Temperatures in Degrees Celcius")


## Exercise 3 : Calculation of the trend as a moving average
#Part A

covid <- read_excel("cases of coronavirus.xlsx", sheet = 2)
View(covid)
attach(covid)


cases<-Number
dates<-as.Date(as.character(Date),"%m/%d/%y")

casesTS<-timeSeries(cases,dates)

plot(casesTS, ylab = "Number", xlab="Time",
     main = "Number of cumulative cases of coronavirus in US 
     from January 20, 2020 to April 8, 2022")

#Part B:
case_diff <- diff(casesTS, lag = 1) #lag = 1 coz yt = Xt - Xt-1
# in that depends on the previous day, or is compared to the prev day.

plot(case_diff)


#Part C
f7 = rep(1/7, 7)
f7

ma7 = filter(case_diff, f7, method = "convolution", sides = 1)

plot(cbind(case_diff,ma7), plot.type="single", 
     ylab = "Number",
     main = "Daily increase of new cases in US
     from January 21, 2020 to April 8, 2022")

incidence <- ma7/331002651 * 100000 * 7

## approximate incidence of cases in the past seven days 
## per 100, 000 population


plot(incidence, ylab = "Incidence", xlab="Time", 
     main = "7 day incidences per 100,000 population")
