#
# We will work with Pfizer stock data since inception
# 
#

# Step 0: Load the lubridate package. This helps manipulate
# date fields, which is critical to time series
library(lubridate)

# This is a library that comes shipped with RStudio, and 
# facilitates interactive plotting
library(manipulate)

# Read from the AAPL (Pfizer) stock data
Pfizer <- read.csv("PFE.csv",
                  header=TRUE)

# Step 1: Need to recognise the Date field formally
# Otherwise R treats it as character data
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

# Step 2: Create a separate variable for the year 
# associated with a date, and append it as a column
# to the Pfizer data frame
Pfizer$Year <- year(Pfizer$Date)

# Step 3: Plot a histogram of adjusted close prices 
# between lowYr and upYr. Interact using the window.
# What do you notice?
lowYr <- 1972; upYr <- 2017
manipulate(
  
  plot(subset(Pfizer, 
              Year >= lowYr & Year <= upYr)$Date, 
       subset(Pfizer, 
              Year >= lowYr & Year <= upYr)$Adj.Close,
       main=paste("PFE from", lowYr, "through", upYr),
       xlab="Year",
       cex=0.2,
       type="l",
       col="darkgreen",
       ylab="Adjusted Close Price"),
  
  lowYr = slider(1972, 2017, 
                     step=1, 
                     initial=1972),
  upYr = slider(lowYr, 2017,
                     step=1,
                     initial=2017))
