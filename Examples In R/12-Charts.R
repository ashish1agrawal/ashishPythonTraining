library(lubridate) # Date manipulation
library(plotrix)   # 3D pie charts

# Read from the PFE (Pfizer) stock data
Pfizer <- read.csv("PFE.csv",
                  header=TRUE)

# Step 1: Need to convert this into Date type
Pfizer$Date <- as.Date(Pfizer$Date, "%Y-%m-%d")

# Step 2: Add a Month column to the frame
Pfizer$Month <- month(Pfizer$Date, label=TRUE)

# Step 3: Pfizer trades past 2007
PfizerSince2007 <- subset(Pfizer,
                          year(Date) >= 2007)

# Step 3: Create a bar chart of the frequencies by month
counts <- table(PfizerSince2007$Month)
barplot(counts, 
        main="Trading days after 2007 for PFE",
        col=c("red","white","blue"))
        # horiz=TRUE)

lbls=c("J","F","M","A","M","J","J","A","S","O","N","D")
pie3D(counts, labels=lbls,
      main="Trading Days")


# Step 4: Let's average the medians by trading month
medians <- aggregate(PfizerSince2007$Adj.Close,
                     by = list(PfizerSince2007$Month),
                     FUN = median)
# print(means)
barplot(medians$x, 
        names=medians$Group.1,
        main="Median Performance over the last 10 years of PFE",
        col=c("orange","white","darkgreen"))

# Step 5: Let's understand volatilaity by trading month
stDev <- aggregate(PfizerSince2007$Adj.Close,
                     by = list(PfizerSince2007$Month),
                     FUN = sd)
# print(stDev)
barplot(stDev$x, 
        names=stDev$Group.1,
        main="Volatility over the last 10 years of PFE",
        col=c("orange","white","darkgreen"))