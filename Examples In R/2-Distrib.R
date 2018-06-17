# Stick two plots next to each other
par(mfrow=c(1, 2))

# Simulating a normal distribution
normvar <- rnorm(100)
hist(normvar,20)   # histogram
#qqnorm(normvar) # QQ-plot

unifvar <- runif(1000)
hist(unifvar)
print(mean(normvar))
print(sd(normvar))
