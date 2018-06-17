# Simulating numbers according to UNIFORM distributions
#
par(mfrow=c(1,2))
par(pty="s")

# Number of numbers to generate
numGen <- 1000 

# Let's begin with a  DISCRETE distribution
# We generate nos. between 1 and 9
numDisc <- sample(1:9, numGen, replace=TRUE)

mp <- hist(numDisc,
           main="1000 DISCRETE Uniform numbers",
           breaks=c(1:10),
           labels=TRUE,
           right=FALSE,
           xlab="Number generated",
           xlim=c(1,10),
           ylim=c(0,round(numGen*0.15)),
           xaxt="n", # Inhibit default x axis
           col=colors()[30:39])

# Now draw a "good" x axis with labels center justified
xLabels <- as.character(c(1:9))
axis(side=1, 
     xLabels, 
     at=mp$mids)

# Next, we move to a  CONTINUOUS distribution
numCont <- runif(numGen, 
                 min=1, 
                 max=9)

mp <- hist(numCont,
           main="1000 CONTINUOUS Uniform numbers",
           breaks=c(0:10),
           labels=TRUE,
           right=FALSE,
           xlab="Number generated",
           xlim=c(0,10),
           #ylim=c(0,round(numGen*0.15)),
           xaxt="n", # Inhibit default x axis
           col=colors()[40:50])

# Again draw a "good" x axis with labels center justified
xLabels <- as.character(c(1:9))
axis(side=1, 
     xLabels)

# Lay on the density curve
par(new=TRUE)
plot(density(numCont),
     axes=FALSE,
     ann=FALSE,
     lwd=2,
     col="red")

