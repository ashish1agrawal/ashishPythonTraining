#
# We will work with Pfizer stock data since inception
# You may choose to look up this page for its history
# http://en.wikipedia.org/wiki/History_of_Pfizer_Inc.
#

# Step 0: Load the lubridate package. This helps manipulate
# date fields, which is critical to time series
library(lubridate)

# Read from the PFE (Pfizer) stock data
Pfizer <- read.csv("PFE.csv",
                  header=TRUE)

# Step 1: Need to recognise the Date field formally
# Otherwise R treats it as character data
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

# Step 2: Create a separate variable  for the year 
# associated with a date, and append it as a column
Pfizer$Year <- year(Pfizer$Date)

# Step 3: Boxplot for 20 years ago - lower outliers!
Year1987 <- subset(Pfizer, Year == 1987)
boxplot(Adj.Close ~ Year, data=Year1987,
        horizontal=TRUE,
        col="orange",
        main="PFE for the Year 1987",
        pch=20, cex=0.7)

# Step 4: Let's work on a combined graph, which depicts
#         the activity for 2016
oldPars <- par(no.readonly=TRUE)

Year2016 <- subset(Pfizer, Year == 2016)
with(Year2016, {
  par(fig=c(0, 0.8, 0, 1))
  plot(Date, Adj.Close,
       main="PFE for Year 2016",
       pch=20,
       cex=0.4,
       ylim=c(26, 37),
       las=1,
       xlab="", ylab="Adjusted Close",
       col="darkgreen")
  abline(h = max(Year2016$Adj.Close),
         col = "orange")
  abline(h = min(Year2016$Adj.Close),
         col = "blue")
  
  par(fig=c(0.6, 1, 0, 1), new=TRUE)
  boxplot(Adj.Close,
          axes=FALSE,
          ylim=c(26, 37),
          col="beige")
})

par(oldPars)

# Step 4: Plot the summaries of Adj.Close in three slabs
slab1 <- subset(Pfizer, Year < 1990)
slab2 <- subset(Pfizer, Year >= 1990 & Year <= 2000)
slab3 <- subset(Pfizer, Year > 2000)

boxplot(Adj.Close ~ Year, data=slab1,
        col=colors()[80:89],
        main="PFE before 1990",
        pch=20, cex=0.7)

boxplot(Adj.Close ~ Year, data=slab2,
        col=colors()[30:45],
        main="PFE between 1990 and 2000",
        pch=20, cex=0.7)

boxplot(Adj.Close ~ Year, data=slab3,
        col=colors()[40:70],
        main="PFE after 2000",
        pch=20, cex=0.7)

