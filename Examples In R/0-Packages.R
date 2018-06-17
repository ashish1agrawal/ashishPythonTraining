install.packages(c("sandwich","rms","Deducer",
                   "ResourceSelection", "ROCR","XML",
                   "cairoDevice", "RGtk2Extras",
                   "rpart", "rattle","rpart.plot",
                   "partykit","xlsx","rJava","xlsxjars",
                   "caret", "e1071","lubridate","plotrix",
                   "xts","manipulate","nycflights13",
                   "Quandl"), dependencies = TRUE)

#install.packages("CHAID")      #install pkg partykit first then on console:
install.packages("CHAID", repos="http://R-Forge.R-project.org", type="source")
library(CHAID)