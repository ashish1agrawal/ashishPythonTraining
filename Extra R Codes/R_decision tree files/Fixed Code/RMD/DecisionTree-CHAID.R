#################################CHAID decision tree####################################
library(partykit)   #needed for CHAID
library(CHAID)      #install pkg partykit first then on console:
                    #install.packages("CHAID", repos="http://R-Forge.R-project.org", type="source")
library(rattle)     #to get the decision tree rules
library(rpart.plot) #to plot the decision trees

#######################Read data from a specified location##############################
HrRawData <- read.csv(file.choose(),head=TRUE,sep=",")

######################Summary of the data on which model is built#######################
str(HrRawData) #information on the HR metadata
HrData <- na.omit(HrRawData) # listwise deletion of missing values

###############################converting class to factors##############################
for(i in c(1:ncol(HrData))) {
  HrData[,i] <- as.factor(HrData[,i])
}

################# define an 75%/25% train/test split of the dataset#####################
bound <- floor((nrow(HrData)/4)*3)                 #define % of training and test set
HrData <- HrData[sample(nrow(HrData)), ]           #sample rows 
HrData.train <- HrData[1:bound, ]                  # training dataset
HrData.test <- HrData[(bound+1):nrow(HrData), ]    # test dataset

#########################grow initial tree using CART of rpart##########################
############################some error here. correct it#################################

ctrl <- chaid_control(minsplit = 50)
ChaidModel <- chaid(Final.HR.Status ~ DOJExtended+OfferedBand+
                   Joining.Bonus.Given+Notice.Period+
                   CandidateSource+LOB+Location
                   , data = HrData.train, control = ctrl)
print(ChaidModel)
plot(ChaidModel)

###################Use fitted model to prediction for test data#######################
predictTestChaid = predict(ChaidModel,HrData.test,type = "response")

############################Create the confusion matrix###############################
table(HrData.test$Final.HR.Status,predictTestChaid)
