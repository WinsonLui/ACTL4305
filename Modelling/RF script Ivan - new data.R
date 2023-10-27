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

# Advantages: lower variance, high accuracy, good in large datasets, uses decollerated trees unlike bagging so does not overfit
# Disadvantages: Low interpretability, computationally expensive, time consuming

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
data["FWI_99th_flag"][data["FWI_99th_flag"] == TRUE] <- 1 #Convert T/F to 1/0 for easier model handling
data["FWI_99th_flag"][data["FWI_99th_flag"] == FALSE] <- 0 #^
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
#bestmtry <- tuneRF(data_train[-c(13)], data_train$Bushfire_Flag, ntreeTry = 500,
#                   mtryStart  = 5,stepFactor = 1.2, improve = 0.01, trace=T) 
bestmtry <- 8
#14 using old data, 8 using new data

# Create RF model
model <- randomForest(Bushfire_Flag~.,data= data_train, mtry=bestmtry)
model #summary
importance(model) # shows how much each factor contributes to GINI index (error) decrease

# Prediction
pred_test <- predict(model, newdata = data_test, type= "vote")
pred_test <- as.data.frame(pred_test) #convert to df
colnames(pred_test)[1] = "prediction" #rename column

# Re-order soft delete (no need to reorder since both datasets are in the same jumbled up order)
#pred_test$index <- as.numeric(row.names(pred_test))
#df <- as.data.frame(pred_test[order(pred_test$index), ]$prediction)
#colnames(df)[1] = "prediction"

# ROC
roc_test<- roc(data_test$Bushfire_Flag, pred_test$prediction)
roc_test # summary and AUC
plot(roc_test) #ROC curve
