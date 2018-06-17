library(ggplot2)
library(nycflights13)
library(lubridate)

XmasEveFlights <- subset(flights, 
                         month == 12 & day == 24)

ggplot(data = XmasEveFlights) +
  geom_point(mapping = aes(x = dep_delay,
                           y = arr_delay))