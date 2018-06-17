# We shall manipulate dates in this exercise
# Step 0: Install the lubridate package
library(lubridate)

# Read from the PFE (Pfizer) stock data
Pfizer <- read.csv(file.choose(), header=TRUE)

p.new <- Pfizer[,c(2:5)]

apply(p.new,1, sd)

# Note: R treats the Date field as factor
print(paste("The initial type of Date is", 
            class(Pfizer$Date)))

# Step 1: Need to convert this into Date type
print("Converting the Date column to date type")
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

# List the first and last few rows of the dataset
print("The first and last sections of the dataset are")
print(head(Pfizer))
print(tail(Pfizer))

Pfizer$Month <- as.factor(month(Pfizer$Date))
Pfizer$Year <- year(Pfizer$Date)

# Step 2: Focus on 2017 stock activity
#         Express it as a classic time series
May2017 <- subset(Pfizer,
                    Month == 5 & 
                    Year == 2017,
                    select = c(Date, Adj.Close))
May2016 <- subset(Pfizer,
                  Month == 5 & 
                    Year == 2016,
                  select = c(Date, Adj.Close))
# Step 3: Set the options for the plot, after making a copy
# of the old settings
oldPars <- par(no.readonly=TRUE)
par(bg="beige")
par(lty="solid")

# Margin in inches: bottom, left, top, right
# par(mai=c(1.5,1.5,1.5,1.5)) 

# Step 4: Graph the time series, after setting parameters
with(May2017, {
  plot(Date, Adj.Close, 
       col="red",
       xlab="Month",
       las=1,
       pch=20) # symbol type
  lines(Date, Adj.Close, col="navy")
  title("Pfizer's Stock Activity for May 2017")
})

with(May2016, {
  plot(Date, Adj.Close, 
       col="red",
       xlab="Month",
       las=1,
       pch=20) # symbol type
  lines(Date, Adj.Close, col="navy")
  title("Pfizer's Stock Activity for May 2016")
})

# Step 5: Restore the old parameter settings
par(oldPars)

