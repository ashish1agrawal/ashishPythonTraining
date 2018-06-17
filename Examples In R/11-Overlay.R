# We shall manipulate dates in this exercise
# Step 0: Install the lubridate package
library(lubridate)

# Read from the PFE (Pfizer) stock data
Pfizer <- read.csv("PFE.csv",
                  header=TRUE)

# Step 1: Need to convert this into Date type
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

# Step 2: Focus on April stock activities for two years
#         Express it as a classic time series
Apr2017 <- subset(Pfizer,
                    month(Date) == 4 & 
                    year(Date) == 2017,
                    select = c(Date, Adj.Close))

Apr2016 <- subset(Pfizer,
                  month(Date) == 4 & 
                  year(Date) == 2016,
                  select = c(Date, Adj.Close))

# Step 3: Set the options for the plot, after making a copy
# of the old settings
oldPars <- par(no.readonly=TRUE)
par(lty="solid")

# Step 4: Graph the two time series
with(Apr2017, {
  plot(day(Date), Adj.Close,
       las=1,
       xlim=c(0, 30),
       ylim=c(30, 35),
       xlab="Day of Month",
       ylab="Adjusted Close Price",
       type="b",
       col="darkgreen",
       pch=20) # symbol type
  title("A Tale of Two Aprils (Pfizer)")
})

# Prompt to overlay the second plot
par(new=TRUE)

with(Apr2016, {
  plot(day(Date), Adj.Close,
       xlim=c(0, 30),
       ylim=c(30,35), # Must be the same as earlier
       ann=FALSE, # Suppress annotating the axes
       axes=FALSE,
       type="b",
       col="brown",
       cex=0.7,
       pch=17) # symbol type
})
# Step 6: Now add a legend
legend(20,32.5,
       c("2017","2016"),
       pch=c(20, 17),
       col=c("darkgreen", "brown"))

# Step 7: Restore the old parameter settings
par(oldPars)