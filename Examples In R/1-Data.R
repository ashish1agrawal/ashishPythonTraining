# This is how you declare a set of numbers
# In this case, from 1 to 10. 
nums <- c(1:10)

# Explicitly declaring a set of numbers
primes <- c(2,3,5,7,11,13,17,19,23,29)

# Some stats on sets of numbers
myMean <- mean(primes)
cat("The mean of the first", length(nums),
    "primes is", myMean, 
    "\n") # This is an end-of-line character

mySD <- sd(primes)
cat("The SD of primes is", "\n",
    round(mySD, 2))

cor_np <- cor(nums,primes)
cat("Correlation of nums vs primes is", "\n",
    round(cor_np, 2))

# Here's a rudimentary plot
plot(nums,primes,
     pch=15, cex=1.5,
     col=colors()[30:40],
     main="Colourful Primes")

# Regression fit
lmFit <- lm(primes ~ nums)
abline(lmFit,
       col="red")
print(summary(lmFit))

text(2,
     25,
     paste(
       "The equation is:\n",
       "prime = ",
       round(lmFit$coefficients[1], 2),
       "+",
       round(lmFit$coefficients[2], 2),
       "* position"
     ))
  
  