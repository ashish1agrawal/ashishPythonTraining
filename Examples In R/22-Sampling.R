#code is imp and can obtained from the site
#https://www.quandl.com/
#code for infy: "NSE/INFY"

library(Quandl)
library(caret)
data = Quandl("NSE/AXISBANK", collapse = "weekly", 
              start_date = "2007-01-01", order = c("asc"))
head(data)
#write.csv(data, "Infy_weekly_stock.csv")
#data1 <- read.csv(file.choose(),head = TRUE, sep = ",")

samDist <- c()
samSD <- c()
for (i in 1:200) {
  sampled <- sample(data[,6],50,replace = TRUE)
  samDist[i] <- mean(sampled)
  samSD[i] <- sd(sampled)
}
dev.off()
hist(data[,6],20)
hist(samDist,20, col = 'blue')

qqnorm(samDist)
qqnorm(data[,6])

mean(data[,6])
sd(data[,6])

mean(samDist)
sd(samSD)
