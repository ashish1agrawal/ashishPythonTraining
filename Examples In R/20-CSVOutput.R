# Several tools have a USP that their users can
# create _multiple_ random samples and
# list them as columns on a spreadsheet
# Here's how we do this with R

# Create the dataset
Pfizer <- read.csv(file.choose(), header=TRUE)

# We want the sample to contain Adjusted Close information
# from the dataset, which occupies the 9th column
numSamples <- 100
sampleSize <- 30
sampleFrom <- nrow(Pfizer)
adjClose <- 6

# In this statement, the sample() portion indicates row info
# and adjClose indicates column info
# Declare a matrix to hold the samples data
samples <- matrix(nrow=sampleSize, ncol=numSamples)
colnames(samples) <- paste("Sample", c(1:numSamples))
rownames(samples) <- paste("Sample Element", c(1:sampleSize))

# Populate a data array with samples of Previous Salaries
for(i in 1:numSamples) {  
  samples[, i] <- Pfizer[sample(sampleFrom, sampleSize),
                         adjClose]
}

# We will now output the samples matrix to an Excel file
fileName <- paste("Samples", numSamples, ".csv", sep="")
write.csv(samples, file=fileName)
cat(paste("Check your directory for the file:", fileName, "\n"))

# Uncomment these lines to witness the Central Limit Theorem
means <- colMeans(samples)
meanSamples <- round(mean(means), 3)
sdSamples <- round(sd(means), 3)
popMean <- round(mean(Pfizer[ , adjClose]), 3)
popSD <- round(sd(Pfizer[ , adjClose]), 3)

par(mfrow=c(1, 2))
hist(means,
     main="",
     breaks=12,
     labels=TRUE,
     las=1,
     ylim=c(0, numSamples/3),
     xlab=bquote(paste("Distribution ", 
                       mu,"=", .(meanSamples), "  ",
                       sigma, "=", .(sdSamples))),
     col=colors()[20:35])
title(main=paste("Means for", numSamples, 
                 "Samples of size n =", sampleSize),
      sub=paste("Population Mean:", popMean, "SD:", popSD))

# Visually check for normality
library(car)
qqPlot(means,
       main=paste("Q-Q plot for n = ", sampleSize),
       pch=18,
       cex=0.6,
       col="navy")
