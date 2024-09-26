# EXERCISE 1
cbe <- read.table(file = "cbe.dat", header = TRUE)
#View(cbe)
attach(cbe)

chocTS <- ts(choc, start = c(1958,1), end = c(1990,12), frequency = 12)
beerTS <- ts(beer, start = c(1958,1), end = c(1990,12), frequency = 12)
elecTS <- ts(elec, start = c(1958,1), end = c(1990,12), frequency = 12)

plot(cbind(chocTS, beerTS, elecTS), main = "Chocolate, Beer and Electricity
     Time Series (1958-1958)")

## Part A
chocTS.decompose <- decompose(chocTS, type = "multiplicative")
beerTS.decompose <- decompose(beerTS, type = "multiplicative")
elecTS.decompose <- decompose(elecTS, type = "multiplicative")

#minus trends
chocTS.z <- chocTS - chocTS.decompose$trend
beerTS.z <- beerTS - beerTS.decompose$trend
elecTS.z <- elecTS - elecTS.decompose$trend

plot(cbind(chocTS.z,beerTS.z,elecTS.z))



## Part B
#Chocolate
mean(chocTS)                  # 4685.255
mean(chocTS.z, na.rm = TRUE)  # -1.629557
sd(chocTS)                    # 1771.239
sd(chocTS.z, na.rm = TRUE)    # 1051.446

#Beer
mean(beerTS)                  # 137.5795
mean(beerTS.z, na.rm = TRUE)  # -0.01857639
sd(beerTS)                    # 33.58404
sd(beerTS.z, na.rm = TRUE)    # 18.5138

#Electricity
mean(elecTS)            # 6311.699
mean(na.omit(elecTS.z)) # 1.422635
sd(elecTS)              # 3382.868
sd(na.omit(elecTS.z))   # 527.5799


# PART C
chocTS.random <- chocTS.decompose$random
plot(chocTS.random, main = " Random Component for Chocolate")

beerTS.random <- beerTS.decompose$random
plot(beerTS.random, main = " Random Component for Beer")

elecTS.random <- elecTS.decompose$random
plot(elecTS.random, main = " Random Component for Electricity")

## Part D
mean(chocTS.random, na.rm = TRUE)  # 0.9995676
sd(chocTS.random, na.rm = TRUE)    # 0.09572697

mean(beerTS.random, na.rm = TRUE)  # 0.999925
sd(beerTS.random, na.rm = TRUE)    # 0.06177597

mean(elecTS.random, na.rm = TRUE)  # 0.9999542
sd(elecTS.random, na.rm = TRUE)    # 0.02005056


## Part E
acf(chocTS,na.action = na.pass, main = "ACF Chocolate TS")
acf(beerTS, na.action = na.pass, main = "ACF Beer TS")
acf(elecTS, na.action = na.pass, main = "ACF Electricity")

acf(chocTS.z, na.action = na.pass, main = "ACF for Chocolate Without Trend")
acf(beerTS.z, na.action = na.pass, main = "ACF for Beer Without Trend")
acf(elecTS.z, na.action = na.pass, main = "ACF for Electricity Without Trend")

acf(chocTS.random, na.action = na.pass, main = "ACF for Random Component Chocolate")
acf(beerTS.random, na.action = na.pass, main = "ACF for Random Component Beer")
acf(elecTS.random, na.action = na.pass, main = "ACF for Random Component Electricity")


# PROBLEM 2
#function to simuate the problem
noiseTS <- function(variance){
  set.seed(5)
  noise <- rnorm(1000, mean = 0, sd = sqrt(variance))
  noiseTS <- ts(noise)
}


#observation, looks same but variance is diff, (check scales)
plot(cbind(noiseTS(10), noiseTS(20), noiseTS(30)), main =  "Gaussian Noise with Variance as 1,2 and 3")
