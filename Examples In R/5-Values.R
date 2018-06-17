# Regress miles/gallon as a linear function of weight
lmfit <- lm(mpg ~ wt, data=mtcars) 

# Print the summary
print(summary(lmfit), digits=3)

# Visualise the regression
plot(mtcars$wt, mtcars$mpg,
     xlab="Weight of car ('000 lbs)",
     ylab="Mileage (mpg)",
     ylim=c(0,40),
     pch=15, cex=0.7,
     col="blue")

title("Regression Plot")

# Overlay the regression line
abline(lmfit, col="red", lwd=3)

# Overlay the fitted vales in red
points(mtcars$wt, lmfit$fitted.values,
       pch=20, cex=1.7,
       col="red")

# plot(lmfit)
coeff1 <- lmfit$coefficients[1]
coeff2 <- lmfit$coefficients[2]

eqn <- paste("mpg =", round(coeff1, 1), "+", 
             round(coeff2, 1), "* wt")
text(2, 40, eqn)
