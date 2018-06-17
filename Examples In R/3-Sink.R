# First, set the working directory
# Two ways to do it
#    1. As an RStudio option
#    2. Using setwd()
#
setwd("/Users/Rahul/Documents/Rahul Office/IIMB/Work @ IIMB/Training Material/Concepts/R files/ExamplesinR")
# Want to seek a character input. scan terminated by blank space and enter.
myInput <- scan(what='character')

# Run this only after the previous command
cat("Here is what you entered:", 
    myInput, "\n\n")

# Next, create a sub-directory for the output
dir.create("SinkEx")

# Start recording the output
sink("SinkEx/NumPrimes.txt", append=TRUE)
pdf("SinkEx/Graphs.pdf")

source("1-Data.R")
source("2-Distrib.R")

# Stop recording the output
sink()
dev.off()
print("We are done!")
