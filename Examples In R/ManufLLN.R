# Simulation of assembly line for a manufacturing line
#
# Let's assume that 70% of the Items work, from historical data
p <- 0.7

# Let's test 100 manufactured items
N <- 100
randVars <- runif(N)
workingItem <- sapply(randVars, 
                      function(x) 
                        if (x < p) 1 else 0)
  
# We simulate a variable that assumes 1 in p proportion of the cases
ItemTest <- data.frame(1:100, workingItem)
colnames(ItemTest) <- c("ItemNumber", "Works")

# Make the background transparent and plot
par(bg=NA)
plot(ItemTest$ItemNumber, ItemTest$Works,
     pch=20, cex=0.7, col="navy",
     xlab="Item Number", ylab="1 = Working, 0 = Not",
     main="")

# Now superimpose the relative frequencies and look for a trend
relFreq <- c()
for(i in 1:N) {
  relFreq[i] <- sum(workingItem[1:i])/i
}

par(new=TRUE)
plot(ItemTest$ItemNumber, relFreq,
     col="red",
     type="l",
     xaxt="n", yaxt="n",
     ylim=c(0, 1),
     ann=F)
abline(h=p, col="darkgreen")

