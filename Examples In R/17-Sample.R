# Let's play with sampling

# Generate a sample of 5 numbers between 10 and 30
min <- 10
max <- 30
mySample <- sample(c(min:max), 5)

cat("The sample generated is:", mySample, "\n")

#
# Next, we will work with Pfizer stock data since inception
# 
#

# Function to build a histogram
makeHist <- function(data, sampleSize) {
  
  hist(data,
       main=paste("Sample of size", sampleSize),
       breaks=20,
       las=1,
       col=colors()[30:50])
  
}

# Step 0: Load the lubridate package. This helps manipulate
# date fields, which is critical to time series
library(lubridate)

# Read from the AAPL (Pfizer) stock data
Pfizer <- read.csv(file.choose(), header=TRUE)

# This is the numerical data we work with
ClosePrices <- Pfizer$Adj.Close

# Pick three samples of varying sizes
par(mfrow=c(2,2))
makeHist(ClosePrices, length(ClosePrices))

sample30 <- sample(ClosePrices, 30)
makeHist(sample30, 30)

sample100 <- sample(ClosePrices, 100)
makeHist(sample100, 100)

sample400 <- sample(ClosePrices, 400)
makeHist(sample400, 400)

