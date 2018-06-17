#-----------------------Fitting regression in R------------------------------
#---------------------read about dummy variable coding at--------------------
#-----------http://www.ats.ucla.edu/stat/mult_pkg/faq/general/dummy.htm------
#-----------http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm-------
#-----------http://www.ats.ucla.edu/stat/r/modules/dummy_vars.htm------------
#----------------------------------------------------------------------------
library(stats)    #for regression lm()
#library(sandwich) #var, covariance matrix

#setwd("~/Desktop/R files")

#######################Read data from a specified location##############################
LinRegDataRaw <- read.csv(file.choose(),head=TRUE,na.strings=c("", " ", "NA"),sep=",") 

######################Summary of the data on which model is built#######################
str(LinRegDataRaw) #information on the metadata
summary(LinRegDataRaw)

######################listwise deletion of missing values###############################
LinRegData <- na.omit(LinRegDataRaw)
LinRegData$loglosses <- log(LinRegData$Losses)
################# define an 80%/20% train/test split of the dataset#####################
#define % of training and test set
#sample rows 
#training dataset
#test dataset

##########################Fit regression model using ###################################
FitLinReg <- lm(loglosses ~ Age + Gender 
               + Married + Vehicle.Age + Fuel.Type,  LinRegData)
#FitLinReg <- lm(Losses ~ .,  LinRegData)

#############################Model Validation###########################################
#The regression estimate of SE makes an assumption about the constant variance w.r.t to 
#the predictors.
summary(FitLinReg)





#####################estimates of the variance and covariance between####################
########################the regression coefficients######################################
#gives the standard error of the beta coefficients of the X variables
#vcov(FitLinReg)
#diag(vcov(FitLinReg))
#sqrt(diag(vcov(FitLinReg)))

#cov2cor(vcov(FitLinReg))

#########################Validation - normality for residuals###########################
res <- resid(FitLinReg) #List of residuals
restan <- rstandard(FitLinReg)
#restan <- scale(FitLinReg$residuals) #another way of standarization
hist(residuals(FitLinReg), main = "Residuals")
qqnorm(res)
qqline(res, col = "red")

######################Validation - check for heteroscedasticity#########################
y <- LinRegData$Losses
yhat <- FitLinReg$fitted.values
plot(predict(FitLinReg), residuals(FitLinReg))
#plot(yhat, res) #same plot as above

#Heteroskedasticity-consistent SE. Use these SE of coeffeicents in case of heteroskedastic 
#issue. More at: https://www.youtube.com/watch?v=hFoDDwTF4KY
#vcovHC(FitLinReg, omega = NULL, Type = "HC3") #var covvariance of variables

################do a prediction on the test data created as part of assignment##########
#Your code goes here

