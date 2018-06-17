library(lubridate)
library(car) #for qqPlot

#To show normal distribution using pfizer data
pfizer <- read.csv(file.choose(), sep = ",")

par(mfrow=c(1,3)) # to make three plot on same row
hist(pfizer$Adj.Close,
     breaks = 30,
     col = colors()[30:50],
     main = " Closes of PFE",
     xlab = "Adj Close Price",
     las = 1)

#Calculating  return as a random variable
nrows <- nrow(pfizer)
returns <- c()
Closes <- pfizer$Adj.Close
for (i in 1:(nrows-1)) {
  returns[i+1] <- (Closes[i+1] - Closes[i]) / Closes[i]
}
returns[1] <- returns[2]
pfizer$Return <- returns

#Plotting  return
hist(pfizer$Return,
     breaks = 30,
     col = colors()[30:50],
     main = " Closes of PFE",
     xlab = " Return",
     las = 1)

qqplot(pfizer$Return,
       main = "Are the return normal",
       pch = 20, cex = 0.7, col="navy")

#to see the observation with return actually zero
subset(pfizer, pfizer$Return == 0)

#to see the observation of the return between mu and sigma
mu <- mean(pfizer$Return)
sigma <- sd(pfizer$Return)
plusMinusSigma <- subset(pfizer, 
                         Return>= (mu-sigma) & Return <= (mu+sigma))

#Return between 10 percent and 15 percent
plusMinusSigma1<- subset(pfizer, 
                         Return>= 0.1 & Return <= 0.15)