library(lubridate) # Date manipulation
library(xts)   # For time series manipulation

# Read from the PFE (Pfizer) stock data
Pfizer <- read.csv(file.choose(), header=TRUE)

# Step 1: Need to convert this into Date type
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

PFE.ts <- xts(Pfizer$Adj.Close, Pfizer$Date)
plot(PFE.ts,
     main="Pfizer Stock over the years")

