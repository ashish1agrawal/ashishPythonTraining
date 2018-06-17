# Modelling 4s in a T-20 cricket series
par(mfrow=c(1,1))
par(pty="m")

# Assume no wides - there are 20 x 6 = 120 balls bowled
n <- 120

# Success probability = historical average no. of 4s per ball
# scored by the team across many matches
probs <- c(0.2, 0.3, 0.4)

# Variable to store distributions with different parameters
dists <- list()

# Maximum for the y axis
maxy <- 0

# Generate the different distributions
for(count in 1:length(probs)) {
  dist <- dbinom(0:120,
                 size=n,
                 prob=probs[count])
  
  maxy <- max(maxy, max(dist))
  dists[[count]] <- dist
}

# Set the colors for the different graphs
colors <- colors()[30:(30+length(probs))]

# Now plot the distributions
for(count in 1:length(probs)) {
  
  plot(dists[[count]],
       pch=count, cex=0.5,
       xlim=c(0,120),
       ylim=c(0, maxy + 0.05),
       xaxt="n", yaxt="n",
       ann=FALSE,
       col=colors[count])
  
  # Stick a vertical line at the mean for the distribution
  abline(v=n*probs[count], col=colors[count]) 
  
  # Overlay the plot on the previous ones
  par(new = TRUE)
}

# Supply the title for the common plots and the axis labels
title(main="Modelling 4s in a T-20 match with the Binomial",
      xlab="No of 4s in a T-20 match",
      ylab="Probability")

# Lay on the axes
axis(1, at=seq(0,120,10))
axis(2, at=seq(0, maxy+0.1, 0.01))

# And finally, supply the legend
text.leg <- c()
for(count in 1:length(probs)) {
  text.leg[count] <- paste("n = ", n, " p = ", probs[count])
}

legend("topright",
       title="Parameters",
       text.leg,
       pch=c(1:length(probs)),
       col=colors)
