library(e1071)                    # load e1071 
duration = faithful$eruptions     # eruption durations 
hist(duration)
print(paste("The skew in the duration of eruptions is",
            skewness(duration)))  
normNums = rnorm(1000)
hist(normNums)
print(paste("The skew in the normal numbers is",
            skewness(normNums)))