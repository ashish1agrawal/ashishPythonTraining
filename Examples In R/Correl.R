##################################################
# Demonstrate plotting graphs of paired variables
# (X1, X2) where cor(X1, X2) varies in a range
#
# Load the library for multivariate 
# normal distribution
library("mvtnorm") 

# Square plots
opar <- par(no.readonly=TRUE)
par(pty="s")
par(mfrow=c(2,3))

##################################################
# A function to plot a dataset with two variables
# such that cor(X1, X2) = cor
plotVars <- function(cor) {
  mu <- c(0, 0) # WLOG, let the means be at the origin
  
  # Given a correlation between X1, X2, we construct
  # a 2 x 2 correlation matrix
  # Recall: The correlation of a variable with itself is 1
  sig <- matrix(c(1,cor,cor,1), 
                byrow=TRUE, ncol=2)
  
  # Simulate a bivariate normal distribution of 
  # 200 datapoints with the stated characteristics
  x <- rmvnorm(n=200, 
               mean=mu, 
               sigma=sig)
  colnames(x) <- c("X1", "X2")
  plot(x, 
       xlab="X1", ylab="X2", 
       xlim=c(-3,3), ylim=c(-3,3),
       pch=20, cex=0.7, col="navy",
       main=paste("Dataset with Correlation", cor))
  
  # Paint the origina red
  points(mu[1], mu[2], pch=7, lwd=2, col="red")
  
  # Draw the axes
  abline(v=mu[1], lty=3)
  abline(h=mu[2], lty=3)
  
  # Should you wish to check, uncomment
#   print("***********************************************")
#   print(paste("Case: Correlation = ", cor))
#   print("The SAMPLE means of the variables are")
#   print(round(colMeans(x), 2))
#   print("The SAMPLE covariance matrix is")
#   print(round(var(x), 2))
}

# Obtain plots for pairs of variables with
# a range of correlation values from -1 to 1
plotVars(1)
plotVars(0.75)
plotVars(0.25)
plotVars(0)
plotVars(-0.5)
plotVars(-1)

# Reset to single plot parameters and graph the function
par(opar)

# Here's a technique to plot any function f(X1)
# First construct a sequence with 200 X1-values
# spanning between -3 and 3
X1 <- seq(-3, 3, length=200)

# Next obtain the function values - the syntax is intuitive
# We get X2 as f(all points in the X1 array)
X2 <- sapply(X1, function(x) { x*x})
correl <- round(cor(X1, X2), 2)

# Be surprised at the correlation number!
plot(X1, X2,
     main=paste("Variables with corelation", correl),
     cex=0.4,
     col="navy")

# Perfect correlation
par(mfrow=c(1,2))
X1 <- seq(-3, 3, 0.05)
plot(X1, 0.5*X1 + 1,
     main="Perfectly correlated variables",
     xlab="X1", ylab="X2",
     pch=20, cex=0.7, col="navy")
abline(h=0, v=0)

plot(X1, -0.5*X1 + 1,
     main="Perfectly correlated variables",
     xlab="X1", ylab="X2",
     pch=20, cex=0.7, col="navy")
abline(h=0, v=0)