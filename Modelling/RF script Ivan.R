# RF Refresher
#1.  Given training data set
#2.  Select number of trees to build (ntrees)
#3.  for i = 1 to ntrees do
#4.  |  Generate a bootstrap sample of the original data
#5.  |  Grow a regression tree to the bootstrapped data
#6.  |  for each split do
#7.  |  | Select m variables at random from all p variables
#8.  |  | Pick the best variable/split-point among the m
#9.  |  | Split the node into two child nodes
#10. |  end
#11. | Use typical tree model stopping criteria to determine when a tree is complete (but do not prune)
#12. end

# Advantages: lower variance, uses decollerated trees unlike bagging
# Disadvantages: Low interpretability, unable to discern effects of predictors

# Script start
# Packages
library(dplyr)
library(caTools)
library(randomForest)
#install.packages('e1071', dependencies=TRUE)
#install.packages('caret', dependencies=TRUE)
library(caret)
library(pROC)
library(ROSE)

# Read data
setwd("C:/Users/Admin/Downloads/ACTL4305/Sandbox Assignment")
data <- read.csv("Data/Data.csv")
summary(data)

# Clean data
data <- na.omit(data) # Remove NA values
data <- data[-c(1)] # Remove date column

# Split into train and test sets, 80:20
split <- sample.split(data, SplitRatio = 0.8) 
data_train <- subset(data, split == "TRUE") 
data_test <- subset(data, split == "FALSE") 

data$Bushfire_Flag <- as.factor(data$Bushfire_Flag)    
data_train$Bushfire_Flag <- as.factor(data_train$Bushfire_Flag)
data_test$Bushfire_Flag <- as.factor(data_test$Bushfire_Flag)

# Look for best mtry
#bestmtry <- tuneRF(data_train[-c(24)], data_train$Bushfire_Flag, ntreeTry = 500,
#                   mtryStart  = 5,stepFactor = 1.2, improve = 0.01, trace=T) 
bestmtry <- 14

# Create RF model
model <- randomForest(Bushfire_Flag~.,data= data_train, mtry=bestmtry)

importance(model) 

# ROC/AUC
pred_test <- predict(model, newdata = data_test, type= "class")

roc_test<- roc(data_test$Bushfire_Flag, pred_test)
auc(roc.test)

