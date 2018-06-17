##################################################
# Shankar Venkatagiri
# Course: Statistics for Business 1
#
# This R "Script" helps you generate
# and compare Bivariate datasets with 
# different correlation values
#
# As correlation decreases in magnitude
# from 1 to 0, the dispersion increases
#
##################################################

##################################################
# DISCLAIMER: Optional material. Please
# observe the action - don't worry about
# the stats behind this one (yet)
##################################################

# You might have to install this package
# Use the Packages tab, it's convenient!
library(manipulate)

# Install this via the Packages tab
# This library helps generate a
# bivariate (normal) dataset
# for a specified correlation
library(mvtnorm)

# Make all plots square in shape
par(pty="s")

# Keep two plots side by side
par(mfrow=c(1, 2))

# Provide a slider to manipulate rho
# rho = Greek symbol for correlation
manipulate(

  # The function rmvnorm generates
  # a bi-variate normal dataset of
  # correlation approximately = rho
  plot(rmvnorm(n = 200,
               mean=c(0,0),
               sig = matrix(c(1, rho, 
                              rho, 1), 
                            byrow=TRUE, 
                            ncol=2)),
       xlab="X", ylab="Y",
       pch=20, cex=0.7, pty=3, 
       col= if(rho<0) "darkred" else "darkgreen",
       main=bquote("Bivariate Normal Data with " ~ 
                     rho == .(rho))),
  rho=slider(-1, 1, step=0.05, initial=-1)
  )