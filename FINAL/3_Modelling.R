
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
#install.packages("pacman")
pacman::p_load("readxl","dplyr", "tidyr", "lubridate", "ggplot2", "hrbrthemes", "ggmap", "broom", "geojsonio", "maptools", "sf", "glmnet", "parallel", "doParallel", "PRROC", "caret", "randomForest", "gbm", "gridExtra", "pROC")

# Load data 
data <- read.table("./Data/Cleaned Data/df.csv", sep=",", header=TRUE)

# data <- data %>% 
#   distinct()

# check <- data %>%
#   group_by(Date,State) %>%
#   summarise(Count=n()) %>%
#   filter(Count >1)

# Process data
data <- data %>% 
  mutate(El_Nino = (SOI > 7)) %>% 
  mutate(La_Nina = (SOI < -7)) %>% 
  mutate(Positive_IOD = (IOD > 0.4)) %>% 
  mutate(Negative_IOD = (IOD < -0.4)) %>% 
  select(-c("FWI_max", "FWI_99th", "FWI_95th", "FWI_95th_flag", "FWI_90th", "FWI_90th_flag", "IOD", "SOI"))

data$Date <- ymd(data$Date)
data$State <- as.factor(data$State)
data$FWI_99th_flag <- as.factor(data$FWI_99th_flag)
data$Bushfire_Flag <- as.factor(ifelse(data$Bushfire_Flag, "Yes", "No"))
data$El_Nino <- as.factor(data$El_Nino)
data$La_Nina <- as.factor(data$La_Nina)
data$Positive_IOD <- as.factor(data$Positive_IOD)
data$Negative_IOD <- as.factor(data$Negative_IOD)

# Prepare Data
set.seed(6)
index <- sample(1:nrow(data), 0.7*nrow(data))

x_train <- data[index, -8][,-1]
y_train <- data[index, 8]
data_train <- data[index,][,-1]

x_test <- data[-index, -8][,-1]
y_test <- data[-index, 8]
data_test <- data[-index,][,-1]

x_train_matrix <- model.matrix(~., x_train)
y_train_matrix <- as.matrix(y_train)

x_test_matrix <- model.matrix(~., x_test)
y_test_matrix <- as.matrix(y_test)


# Balanced Data
set.seed(6)
data_train_balanced <- upSample(x=x_train, y=y_train) %>% 
  rename('Bushfire_Flag' = 'Class')

x_train_balanced <- data_train_balanced[, -11]
y_train_balanced <- data_train_balanced[, 11]

x_train_matrix_balanced <- model.matrix(~., x_train_balanced)
y_train_matrix_balanced <- as.matrix(y_train_balanced)

# 1. GLM Models
## 1(1)
## 1A. Full GLM Model 
LogisticModel <- glm(formula = Bushfire_Flag ~ .,
                data = data_train,
                family = "binomial")

LogisticModel_balanced <- glm(formula = Bushfire_Flag ~ .,
                     data = data_train_balanced,
                     family = "binomial")

summary(LogisticModel)
summary(LogisticModel_balanced)

## 1B. Shrinkage GLM Model
# Run cross-validation to find the optimal the regularization parameter(lambda).
# - alpha=1 is the lasso penalty,
# - alpha=0 the ridge penalty,
# - alpha=0.5 the elasticnet penalty,

# Starting parallel computing
cl <- makeCluster(detectCores()-1) # this detects the cores of your computer

registerDoParallel(cl) # start the parallel computing mode to speed up the CV process.

# 1B1. Lasso penalty
CV_lasso <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                      alpha = 1, nfolds = 10, parallel = T)
CV_lasso_balanced <- cv.glmnet(x_train_matrix_balanced, y_train_matrix_balanced, family="binomial", type.measure = "auc",
                      alpha = 1, nfolds = 10, parallel = T)


# 1B2. Ridge penalty
CV_ridge <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                      alpha = 0, nfolds = 10, parallel = T)
CV_ridge_balanced <- cv.glmnet(x_train_matrix_balanced, y_train_matrix_balanced, family="binomial", type.measure = "auc",
                      alpha = 0, nfolds = 10, parallel = T)

# 1B3. Elasticnet penalty
CV_EN <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                   alpha = 0.5, nfolds = 10, parallel = T)
CV_EN_balanced <- cv.glmnet(x_train_matrix_balanced, y_train_matrix_balanced, family="binomial", type.measure = "auc",
                   alpha = 0.5, nfolds = 10, parallel = T)

stopCluster(cl) # ending parallel computing - important, otherwise R can get funky.

### Visualisation between regularisation parameter (lambda) and model coefficient
plot(CV_lasso$glmnet.fit, xvar = "lambda",main="Lasso")
plot(CV_ridge$glmnet.fit, xvar = "lambda",main="Ridge")
plot(CV_EN$glmnet.fit,    xvar = "lambda",main="Elasticnet")

plot(CV_lasso_balanced$glmnet.fit, xvar = "lambda",main="Lasso_balanced")
plot(CV_ridge_balanced$glmnet.fit, xvar = "lambda",main="Ridge_balanced")
plot(CV_EN_balanced$glmnet.fit,    xvar = "lambda",main="Elasticnet_balanced")

# 1C. Make predictions
prediction_Logistic <- predict(LogisticModel, newdata=data_test, type="response")
prediction_Logistic_balanced <- predict(LogisticModel_balanced, newdata=data_test, type="response")

prediction_lasso <- predict(CV_lasso, s=CV_lasso$lambda.min, newx=x_test_matrix, type="response")
prediction_ridge <- predict(CV_ridge, s=CV_ridge$lambda.min, newx=x_test_matrix, type="response")
prediction_EN <- predict(CV_EN, s=CV_EN$lambda.min, newx=x_test_matrix, type="response")
prediction_lasso_balanced <- predict(CV_lasso_balanced, s=CV_lasso_balanced$lambda.min, newx=x_test_matrix, type="response")
prediction_ridge_balanced <- predict(CV_ridge_balanced, s=CV_ridge_balanced$lambda.min, newx=x_test_matrix, type="response")
prediction_EN_balanced <- predict(CV_EN_balanced, s=CV_EN_balanced$lambda.min, newx=x_test_matrix, type="response")

# Confusion matrix
PredClass_Logistic <- case_when(as.numeric(prediction_Logistic)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_Logistic_balanced <- case_when(as.numeric(prediction_Logistic_balanced)>0.5 ~ "Yes", TRUE ~ "No")

PredClass_lasso <- case_when(as.numeric(prediction_lasso)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_ridge <- case_when(as.numeric(prediction_ridge)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_EN <- case_when(as.numeric(prediction_EN)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_lasso_balanced <- case_when(as.numeric(prediction_lasso_balanced)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_ridge_balanced <- case_when(as.numeric(prediction_ridge_balanced)>0.5 ~ "Yes", TRUE ~ "No")
PredClass_EN_balanced <- case_when(as.numeric(prediction_EN_balanced)>0.5 ~ "Yes", TRUE ~ "No")


ConfuMatrix_Logistic<- confusionMatrix(as.factor(PredClass_Logistic), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_Logistic_balanced<- confusionMatrix(as.factor(PredClass_Logistic_balanced), data_test$Bushfire_Flag, positive="Yes")

ConfuMatrix_lasso<- confusionMatrix(as.factor(PredClass_lasso), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_ridge<- confusionMatrix(as.factor(PredClass_ridge), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_EN<- confusionMatrix(as.factor(PredClass_EN), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_lasso_balanced<- confusionMatrix(as.factor(PredClass_lasso_balanced), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_ridge_balanced<- confusionMatrix(as.factor(PredClass_ridge_balanced), data_test$Bushfire_Flag, positive="Yes")
ConfuMatrix_EN_balanced<- confusionMatrix(as.factor(PredClass_EN_balanced), data_test$Bushfire_Flag, positive="Yes")


# 2. Tree-based Models
fitcontrol <- trainControl(method = "cv", 
                           number = 5,
                           savePredictions = TRUE,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)

# 2A. Decision Tree
# 2A. Decision Tree (with rpart)
set.seed(6)
Tree_default <- train(Bushfire_Flag ~., data = data_train, method = "rpart", metric = "ROC",
               trControl = fitcontrol, tuneLength = 10)
Tree_default_balanced <- train(Bushfire_Flag ~., data = data_train_balanced, method = "rpart", metric = "ROC",
                      trControl = fitcontrol, tuneLength = 10)

print(Tree_default)
plot(Tree_default)
print(Tree_default_balanced)
plot(Tree_default_balanced)

#Note the threshold probability for the classification is 50% by default.
Tree_default_pred <- predict(Tree_default, newdata = x_test, type = "raw") 
Tree_default_conf <- confusionMatrix(Tree_default_pred,  y_test, positive="Yes")

Tree_default_probpred <- predict(Tree_default, newdata = x_test, type = "prob")
Tree_default_auc <- roc.curve(scores.class0 = Tree_default_probpred$Yes, 
                       weights.class0 = as.numeric(y_test)-1, curve = T)
Tree_default_auc$auc

Tree_default_pred_balanced <- predict(Tree_default_balanced, newdata = x_test, type = "raw") 
Tree_default_conf_balanced <- confusionMatrix(Tree_default_pred_balanced,  y_test, positive="Yes")

Tree_default_probpred_balanced <- predict(Tree_default_balanced, newdata = x_test, type = "prob")
Tree_default_auc_balanced <- roc.curve(scores.class0 = Tree_default_probpred_balanced$Yes, 
                              weights.class0 = as.numeric(y_test)-1, curve = T)
Tree_default_auc_balanced$auc

# 2B. Decision Tree (with rpart2)
# set.seed(6)
# Tree_default2 <- train(Bushfire_Flag ~., data = data_train, method = "rpart2", metric = "ROC",
#                trControl = fitcontrol, tuneLength = 10)
# 
# print(Tree_default2)
# plot(Tree_default2)
# 
# Tree_default2_pred <- predict(Tree_default2, newdata = x_test, type = "raw")
# Tree_default2_conf <- confusionMatrix(Tree_default2_pred,  y_test, positive="Yes")
# 
# Tree_default2_probpred <- predict(Tree_default2, newdata = x_test, type = "prob")
# Tree_default2_auc <- roc.curve(scores.class0 = Tree_default2_probpred$Yes, 
#                        weights.class0 = as.numeric(y_test)-1, curve = T)
# Tree_default2_auc$auc


# 2C. Bagging
set.seed(6)
Bag <- train(Bushfire_Flag ~., data = data_train, method = "treebag", metric = "ROC",
             trControl = fitcontrol)
Bag_balanced <- train(Bushfire_Flag ~., data = data_train_balanced, method = "treebag", metric = "ROC",
             trControl = fitcontrol)
print(Bag)
print(Bag_balanced)

Bag_pred <- predict(Bag, newdata = x_test, type = "raw")
Bag_conf <- confusionMatrix(Bag_pred,  y_test, positive="Yes")

Bag_probpred <- predict(Bag, newdata =  x_test, type = "prob")
Bag_auc <- roc.curve(scores.class0 = Bag_probpred$Yes, 
                     weights.class0 = as.numeric(y_test)-1, curve = T)
Bag_auc$auc

Bag_pred_balanced <- predict(Bag_balanced, newdata = x_test, type = "raw")
Bag_conf_balanced <- confusionMatrix(Bag_pred_balanced,  y_test, positive="Yes")

Bag_probpred_balanced <- predict(Bag_balanced, newdata =  x_test, type = "prob")
Bag_auc_balanced <- roc.curve(scores.class0 = Bag_probpred_balanced$Yes, 
                     weights.class0 = as.numeric(y_test)-1, curve = T)
Bag_auc_balanced$auc

# 2D. Random Forest 

set.seed(6)

registerDoParallel(cl) # start the parallel computing mode to speed up the CV process.

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

RF <- train(Bushfire_Flag ~., data = data_train, method = "rf", metric = "ROC",
            trControl = fitcontrol, tuneLength = 10)
RF_balanced <- train(Bushfire_Flag ~., data = data_train_balanced, method = "rf", metric = "ROC",
            trControl = fitcontrol, tuneLength = 10)

stopCluster(cl) # ending parallel computing - important, otherwise R can get funky.

print(RF)
plot(RF)
print(RF_balanced)
plot(RF_balanced)

RF_pred <- predict(RF, newdata = x_test, type = "raw")
RF_conf <- confusionMatrix(RF_pred, y_test, positive="Yes")

RF_probpred <- predict(RF, newdata = x_test, type = "prob")
RF_auc <- roc.curve(scores.class0 = RF_probpred$Yes, 
                    weights.class0 = as.numeric(y_test)-1, curve = T)
RF_auc$auc

RF_pred_balanced <- predict(RF_balanced, newdata = x_test, type = "raw")
RF_conf_balanced <- confusionMatrix(RF_pred_balanced, y_test, positive="Yes")

RF_probpred_balanced <- predict(RF_balanced, newdata = x_test, type = "prob")
RF_auc_balanced <- roc.curve(scores.class0 = RF_probpred_balanced$Yes, 
                    weights.class0 = as.numeric(y_test)-1, curve = T)
RF_auc_balanced$auc



# # Look for best mtry
# bestmtry <- tuneRF(x_train, y_train, ntreeTry = 500,
#                   mtryStart  = 5,stepFactor = 1.2, improve = 0.01, trace=T)
# 
# bestmtry <- 6
# #14 using old data, 8 using new data
# 
# # Create RF model
# RF <- randomForest(Bushfire_Flag~.,data= data_train, mtry=bestmtry)
# RF #summary
# importance(RF) # shows how much each factor contributes to GINI index (error) decrease
# 
# # Prediction
# prediction_RF <- predict(RF, newdata = x_test, type="prob")
# prediction_RF <- as.data.frame(prediction_RF) #convert to df
# # colnames(pred_test)[1] = "prediction" #rename column
# 
# # Re-order soft delete (no need to reorder since both datasets are in the same jumbled up order)
# #pred_test$index <- as.numeric(row.names(pred_test))
# #df <- as.data.frame(pred_test[order(pred_test$index), ]$prediction)
# #colnames(df)[1] = "prediction"
# 
# # ROC
# ROC_RF<-PRROC::roc.curve(scores.class0 = prediction_RF$Yes,
#                                weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)
# ROC_RF$auc # summary and AUC

# 3. Gradient Boosting Machine (GBM)
# 3A. GBM (default grid value)
# 3A1. Define control parameters for train

### Fix for "Error in summary.connection(connection) : invalid connection": https://stackoverflow.com/questions/64519640/error-in-summary-connectionconnection-invalid-connection
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

fitcontrol <- trainControl(method = "cv", 
                           number = 5,
                           savePredictions = TRUE,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)

set.seed(6)
GBM_default <- train(Bushfire_Flag ~ ., data = data_train, method="gbm", distribution = "bernoulli", 
              metric = "ROC", trControl = fitcontrol, verbose = FALSE)

GBM_default_balanced <- train(Bushfire_Flag ~ ., data = data_train_balanced, method="gbm", distribution = "bernoulli", 
                     metric = "ROC", trControl = fitcontrol, verbose = FALSE)


print(GBM_default)
print(GBM_default_balanced)

GBM_default_pred <- predict(GBM_default, newdata = x_test, type = "raw")
GBM_default_conf <- confusionMatrix(GBM_default_pred, y_test, positive="Yes")

GBM_default_probpred <- predict(GBM_default, newdata = x_test, type = "prob")
GBM_default_auc <- roc.curve(scores.class0 = GBM_default_probpred$Yes, 
                      weights.class0 = as.numeric(y_test)-1, curve = T)
GBM_default_auc$auc

GBM_default_pred_balanced <- predict(GBM_default_balanced, newdata = x_test, type = "raw")
GBM_default_conf_balanced <- confusionMatrix(GBM_default_pred_balanced, y_test, positive="Yes")

GBM_default_probpred_balanced <- predict(GBM_default_balanced, newdata = x_test, type = "prob")
GBM_default_auc_balanced <- roc.curve(scores.class0 = GBM_default_probpred_balanced$Yes, 
                             weights.class0 = as.numeric(y_test)-1, curve = T)
GBM_default_auc_balanced$auc

# 3B. GBM (with grid search)
# parameters <- expand.grid(n.trees =  (1:10)*500,
#                           interaction.depth = c(4,5,6),
#                           shrinkage = c(0.01, 0.1),
#                           n.minobsinnode = 10)
# 
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# 
# set.seed(6)
# GBM_gridfit <- train(Bushfire_Flag ~ ., 
#                      data = data_train, 
#                      method = "gbm", 
#                      distribution = "bernoulli",
#                      metric = "ROC",
#                      trControl = fitcontrol, 
#                      tuneGrid = parameters, 
#                      verbose = FALSE)
# stopCluster(cl)
# 
# print(GBM_gridfit$bestTune)
# plot(GBM_gridfit)

# The result shows that the optimal parameters are n.tree = 2000, interaction.deph = 5, shrinkage = 0.1, n.minobsinnode = 10. 

set.seed(6)
fitControl_final <- trainControl(method = "none", classProbs = TRUE)

GBM_best <- train(Bushfire_Flag ~ ., 
                   data = data_train, 
                   method = "gbm", 
                   distribution = "bernoulli",
                   metric = "ROC",
                   trControl = fitControl_final,
                   ## Only a single model can be passed to the
                   ## function when no resampling is used:
                   tuneGrid = data.frame(interaction.depth = 5,
                                         n.trees = 2000,
                                         shrinkage = 0.1,
                                         n.minobsinnode = 10),
                   verbose = FALSE)

GBM_best_balanced <- train(Bushfire_Flag ~ ., 
                  data = data_train_balanced, 
                  method = "gbm", 
                  distribution = "bernoulli",
                  metric = "ROC",
                  trControl = fitControl_final,
                  ## Only a single model can be passed to the
                  ## function when no resampling is used:
                  tuneGrid = data.frame(interaction.depth = 5,
                                        n.trees = 2000,
                                        shrinkage = 0.1,
                                        n.minobsinnode = 10),
                  verbose = FALSE)

GBM_best_pred_balanced <- predict(GBM_best_balanced, newdata = x_test, type = "raw")
GBM_best_conf_balanced <- confusionMatrix(GBM_best_pred_balanced, y_test, positive="Yes")

GBM_best_probpred_balanced <- predict(GBM_best_balanced, newdata = x_test, type = "prob")
GBM_best_auc_balanced <- roc.curve(scores.class0 = GBM_best_probpred_balanced$Yes, 
                                   weights.class0 = as.numeric(y_test)-1, curve = T)
GBM_best_auc_balanced$auc

# # 3B. GBM (using the gbm package)
# # Adjust the format of Y for gbm
# data2 <- data 
# data2$Bushfire_Flag <- as.numeric(data2$Bushfire_Flag)-1
# 
# data2_train <- data2[index,][,-1]
# data2_test <- data2[-index,][,-1]
# 
# set.seed(6)
# GBM2 <- gbm(formula = Bushfire_Flag ~ .,
#             data = data2_train,
#             distribution = "bernoulli",
#             n.trees = 20000,
#             interaction.depth = 1,
#             shrinkage = 0.01,
#             cv.folds = 5,
#             n.cores = NULL, # will use all cores by default
#             verbose = FALSE)
# # print results
# print(GBM2)
# 
# # get the minimum Bernoulli deviance
# min(GBM2$cv.error)
# 
# # plot loss function as a result of n trees added to the ensemble
# gbm.perf(GBM2, method = "cv") 
# #The black curve is training error;
# #the green curve is CV error;
# #the vertical blue dashed line represents the optimal number of trees=9996.



# 4. Model Evaluation
# ROC curve
ROC_lasso<-PRROC::roc.curve(scores.class0 = prediction_lasso,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_ridge<-PRROC::roc.curve(scores.class0 = prediction_ridge,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_EN<-PRROC::roc.curve(scores.class0 = prediction_EN,
                         weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Logistic<-PRROC::roc.curve(scores.class0 = prediction_Logistic,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Tree_default <-PRROC::roc.curve(scores.class0 = Tree_default_probpred$Yes,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Bag<-PRROC::roc.curve(scores.class0 = Bag_probpred$Yes,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_RF<-PRROC::roc.curve(scores.class0 = RF_probpred$Yes,
                          weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_GBM_default<-PRROC::roc.curve(scores.class0 = GBM_default_probpred$Yes,
                                  weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_GBM_best<-PRROC::roc.curve(scores.class0 = GBM_best_probpred$Yes,
                                  weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

SuncorpPalette <- c("#ed215d", "#004751", "#008B92", "#FFE781", "#BFA317", "#0072B2", "#63732C", "#F2B90C","#8AA3A6")
                    


plot(ROC_lasso,color = SuncorpPalette[1], main="ROC curves", auc.main = F, lwd=2)
plot(ROC_ridge,color =SuncorpPalette[2],add=T, lwd=2)
plot(ROC_EN,color =SuncorpPalette[3],add=T, lwd=2)
plot(ROC_Logistic,color =SuncorpPalette[4],add=T, lwd=2)
plot(ROC_Tree_default,color =SuncorpPalette[5],add=T, lwd=2)
plot(ROC_Bag,color =SuncorpPalette[6],add=T, lwd=2)
plot(ROC_RF,add=T,color =SuncorpPalette[7], lwd=2)
plot(ROC_GBM_default,color =SuncorpPalette[8],add=T, lwd=2)
plot(ROC_GBM_best,color =SuncorpPalette[9],add=T, lwd=2)
legend("bottomright", legend = c("Lasso", "Ridge","EN", "Simple logistic", "CART", "Bagging", "Random forest", "GBM (default)", "GBM (best)"),
       lwd = 2, col = SuncorpPalette)

# ROC curve (balanced)
ROC_lasso_balanced<-PRROC::roc.curve(scores.class0 = prediction_lasso_balanced,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_ridge_balanced<-PRROC::roc.curve(scores.class0 = prediction_ridge_balanced,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_EN_balanced<-PRROC::roc.curve(scores.class0 = prediction_EN_balanced,
                         weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Logistic_balanced<-PRROC::roc.curve(scores.class0 = prediction_Logistic_balanced,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Tree_default_balanced <-PRROC::roc.curve(scores.class0 = Tree_default_probpred_balanced$Yes,
                                    weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Bag_balanced<-PRROC::roc.curve(scores.class0 = Bag_probpred_balanced$Yes,
                          weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_RF_balanced<-PRROC::roc.curve(scores.class0 = RF_probpred_balanced$Yes,
                         weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_GBM_default_balanced<-PRROC::roc.curve(scores.class0 = GBM_default_probpred_balanced$Yes,
                                  weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_GBM_best_balanced<-PRROC::roc.curve(scores.class0 = GBM_best_probpred_balanced$Yes,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

SuncorpPalette <- c("#ed215d", "#004751", "#008B92", "#FFE781", "#BFA317", "#0072B2", "#63732C", "#F2B90C","#8AA3A6")



plot(ROC_lasso_balanced,color = SuncorpPalette[1], main="ROC curves (with balancing)", auc.main = F, lwd=2)
plot(ROC_ridge_balanced,color =SuncorpPalette[2],add=T, lwd=2)
plot(ROC_EN_balanced,color =SuncorpPalette[3],add=T, lwd=2)
plot(ROC_Logistic_balanced,color =SuncorpPalette[4],add=T, lwd=2)
plot(ROC_Tree_default_balanced,color =SuncorpPalette[5],add=T, lwd=2)
plot(ROC_Bag_balanced,color =SuncorpPalette[6],add=T, lwd=2)
plot(ROC_RF_balanced,add=T,color =SuncorpPalette[7], lwd=2)
plot(ROC_GBM_default_balanced,color =SuncorpPalette[8],add=T, lwd=2)
plot(ROC_GBM_best_balanced,color =SuncorpPalette[9],add=T, lwd=2)
legend("bottomright", legend = c("Lasso", "Ridge","EN", "Simple logistic", "CART", "Bagging", "Random forest", "GBM (default)", "GBM (best)"),
       lwd = 2, col = SuncorpPalette)

# # ROC
# ROC_lasso$auc #Lasso
# ROC_ridge$auc #Ridge
# ROC_EN$auc #EN
# ROC_Logistic$auc #simple logistic 




# predict(CV_lasso, s=CV_lasso$lambda.min, newx=x_test_matrix, type="response")






# Find F-scores for each fitted model
Logistic_fscore <- (2*ConfuMatrix_Logistic$byClass["Sensitivity"]*ConfuMatrix_Logistic$byClass["Pos Pred Value"])/(ConfuMatrix_Logistic$byClass["Sensitivity"] + ConfuMatrix_Logistic$byClass["Pos Pred Value"])
Lasso_fscore <- (2*ConfuMatrix_lasso$byClass["Sensitivity"]*ConfuMatrix_lasso$byClass["Pos Pred Value"])/(ConfuMatrix_lasso$byClass["Sensitivity"] + ConfuMatrix_lasso$byClass["Pos Pred Value"])
Ridge_fscore <- (2*ConfuMatrix_ridge$byClass["Sensitivity"]*ConfuMatrix_ridge$byClass["Pos Pred Value"])/(ConfuMatrix_ridge$byClass["Sensitivity"] + ConfuMatrix_ridge$byClass["Pos Pred Value"])
EN_fscore <- (2*ConfuMatrix_EN$byClass["Sensitivity"]*ConfuMatrix_EN$byClass["Pos Pred Value"])/(ConfuMatrix_EN$byClass["Sensitivity"] + ConfuMatrix_EN$byClass["Pos Pred Value"])
Tree_default_fscore <- (2*Tree_default_conf$byClass["Sensitivity"]*Tree_default_conf$byClass["Pos Pred Value"])/(Tree_default_conf$byClass["Sensitivity"] + Tree_default_conf$byClass["Pos Pred Value"])
Bag_fscore <- (2*Bag_conf$byClass["Sensitivity"]*Bag_conf$byClass["Pos Pred Value"])/(Bag_conf$byClass["Sensitivity"] + Bag_conf$byClass["Pos Pred Value"])
RF_fscore <- (2*RF_conf$byClass["Sensitivity"]*RF_conf$byClass["Pos Pred Value"])/(RF_conf$byClass["Sensitivity"] + RF_conf$byClass["Pos Pred Value"])
GBM_default_fscore <- (2*GBM_default_conf$byClass["Sensitivity"]*GBM_default_conf$byClass["Pos Pred Value"])/(GBM_default_conf$byClass["Sensitivity"] + GBM_default_conf$byClass["Pos Pred Value"])
GBM_best_fscore <- (2*GBM_best_conf$byClass["Sensitivity"]*GBM_best_conf$byClass["Pos Pred Value"])/(GBM_best_conf$byClass["Sensitivity"] + GBM_best_conf$byClass["Pos Pred Value"])


# Create a data frame to store the results
model_comparison <- data.frame(
  Model = c("Logistic Regression", "Lasso Regularisation", "Ridge Regularisation", "Elastic Net Regularisation", "Tree_default", "Bagging", "Random Forest", "GBM (default)", "GBM (best)"),
  Accuracy = c(ConfuMatrix_Logistic$overall['Accuracy'], ConfuMatrix_lasso$overall['Accuracy'], ConfuMatrix_ridge$overall['Accuracy'], ConfuMatrix_EN$overall['Accuracy'], Tree_default_conf$overall['Accuracy'], Bag_conf$overall['Accuracy'], RF_conf$overall['Accuracy'], GBM_default_conf$overall['Accuracy'], GBM_best_conf$overall['Accuracy']),
  AUC = c(ROC_Logistic$auc, ROC_lasso$auc, ROC_ridge$auc, ROC_EN$auc, Tree_default_auc$auc, Bag_auc$auc, RF_auc$auc, GBM_default_auc$auc, GBM_best_auc$auc),
  Sensitivity = c(ConfuMatrix_Logistic$byClass['Sensitivity'], ConfuMatrix_lasso$byClass['Sensitivity'], ConfuMatrix_ridge$byClass['Sensitivity'], ConfuMatrix_EN$byClass['Sensitivity'], Tree_default_conf$byClass['Sensitivity'], Bag_conf$byClass['Sensitivity'], RF_conf$byClass['Sensitivity'], GBM_default_conf$byClass['Sensitivity'], GBM_best_conf$byClass['Sensitivity']),
  Specificity = c(ConfuMatrix_Logistic$byClass['Specificity'], ConfuMatrix_lasso$byClass['Specificity'], ConfuMatrix_ridge$byClass['Specificity'], ConfuMatrix_EN$byClass['Specificity'], Tree_default_conf$byClass['Specificity'], Bag_conf$byClass['Specificity'], RF_conf$byClass['Specificity'], GBM_default_conf$byClass['Specificity'], GBM_best_conf$byClass['Specificity']),
  F_score = c(Logistic_fscore, Lasso_fscore, Ridge_fscore, EN_fscore, Tree_default_fscore, Bag_fscore, RF_fscore, GBM_default_fscore, GBM_best_fscore)
)



# Find F-scores for each fitted model
Logistic_fscore_balanced <- (2*ConfuMatrix_Logistic_balanced$byClass["Sensitivity"]*ConfuMatrix_Logistic_balanced$byClass["Pos Pred Value"])/(ConfuMatrix_Logistic_balanced$byClass["Sensitivity"] + ConfuMatrix_Logistic_balanced$byClass["Pos Pred Value"])
Lasso_fscore_balanced <- (2*ConfuMatrix_lasso_balanced$byClass["Sensitivity"]*ConfuMatrix_lasso_balanced$byClass["Pos Pred Value"])/(ConfuMatrix_lasso_balanced$byClass["Sensitivity"] + ConfuMatrix_lasso_balanced$byClass["Pos Pred Value"])
Ridge_fscore_balanced <- (2*ConfuMatrix_ridge_balanced$byClass["Sensitivity"]*ConfuMatrix_ridge_balanced$byClass["Pos Pred Value"])/(ConfuMatrix_ridge_balanced$byClass["Sensitivity"] + ConfuMatrix_ridge_balanced$byClass["Pos Pred Value"])
EN_fscore_balanced <- (2*ConfuMatrix_EN_balanced$byClass["Sensitivity"]*ConfuMatrix_EN_balanced$byClass["Pos Pred Value"])/(ConfuMatrix_EN_balanced$byClass["Sensitivity"] + ConfuMatrix_EN_balanced$byClass["Pos Pred Value"])
Tree_default_fscore_balanced <- (2*Tree_default_conf_balanced$byClass["Sensitivity"]*Tree_default_conf_balanced$byClass["Pos Pred Value"])/(Tree_default_conf_balanced$byClass["Sensitivity"] + Tree_default_conf_balanced$byClass["Pos Pred Value"])
Bag_fscore_balanced <- (2*Bag_conf_balanced$byClass["Sensitivity"]*Bag_conf_balanced$byClass["Pos Pred Value"])/(Bag_conf_balanced$byClass["Sensitivity"] + Bag_conf_balanced$byClass["Pos Pred Value"])
RF_fscore_balanced <- (2*RF_conf_balanced$byClass["Sensitivity"]*RF_conf_balanced$byClass["Pos Pred Value"])/(RF_conf_balanced$byClass["Sensitivity"] + RF_conf_balanced$byClass["Pos Pred Value"])
GBM_default_fscore_balanced <- (2*GBM_default_conf_balanced$byClass["Sensitivity"]*GBM_default_conf_balanced$byClass["Pos Pred Value"])/(GBM_default_conf_balanced$byClass["Sensitivity"] + GBM_default_conf_balanced$byClass["Pos Pred Value"])
GBM_best_fscore_balanced <- (2*GBM_best_conf_balanced$byClass["Sensitivity"]*GBM_best_conf_balanced$byClass["Pos Pred Value"])/(GBM_best_conf_balanced$byClass["Sensitivity"] + GBM_best_conf_balanced$byClass["Pos Pred Value"])


# Create a data frame to store the results
model_comparison_balanced <- data.frame(
  Model = c("Logistic Regression", "Lasso Regularisation", "Ridge Regularisation", "Elastic Net Regularisation", "Tree_default", "Bagging", "Random Forest", "GBM (default)", "GBM (best)"),
  Accuracy = c(ConfuMatrix_Logistic_balanced$overall['Accuracy'], ConfuMatrix_lasso_balanced$overall['Accuracy'], ConfuMatrix_ridge_balanced$overall['Accuracy'], ConfuMatrix_EN_balanced$overall['Accuracy'], Tree_default_conf_balanced$overall['Accuracy'], Bag_conf_balanced$overall['Accuracy'], RF_conf_balanced$overall['Accuracy'], GBM_default_conf_balanced$overall['Accuracy'], GBM_best_conf_balanced$overall['Accuracy']),
  AUC = c(ROC_Logistic_balanced$auc, ROC_lasso_balanced$auc, ROC_ridge_balanced$auc, ROC_EN_balanced$auc, Tree_default_auc_balanced$auc, Bag_auc_balanced$auc, RF_auc_balanced$auc, GBM_default_auc_balanced$auc, GBM_best_auc_balanced$auc),
  Sensitivity = c(ConfuMatrix_Logistic_balanced$byClass['Sensitivity'], ConfuMatrix_lasso_balanced$byClass['Sensitivity'], ConfuMatrix_ridge_balanced$byClass['Sensitivity'], ConfuMatrix_EN_balanced$byClass['Sensitivity'], Tree_default_conf_balanced$byClass['Sensitivity'], Bag_conf_balanced$byClass['Sensitivity'], RF_conf_balanced$byClass['Sensitivity'], GBM_default_conf_balanced$byClass['Sensitivity'], GBM_best_conf_balanced$byClass['Sensitivity']),
  Specificity = c(ConfuMatrix_Logistic_balanced$byClass['Specificity'], ConfuMatrix_lasso_balanced$byClass['Specificity'], ConfuMatrix_ridge_balanced$byClass['Specificity'], ConfuMatrix_EN_balanced$byClass['Specificity'], Tree_default_conf_balanced$byClass['Specificity'], Bag_conf_balanced$byClass['Specificity'], RF_conf_balanced$byClass['Specificity'], GBM_default_conf_balanced$byClass['Specificity'], GBM_best_conf_balanced$byClass['Specificity']),
  F_score = c(Logistic_fscore_balanced, Lasso_fscore_balanced, Ridge_fscore_balanced, EN_fscore_balanced, Tree_default_fscore_balanced, Bag_fscore_balanced, RF_fscore_balanced, GBM_default_fscore_balanced, GBM_best_fscore_balanced)
)









































# Model Scoring
data_1yr <- data %>% 
  filter(Date > as.Date("2022-06-30")) %>% 
  group_by(State) %>% 
  summarise(# FWI_mean = mean(FWI_mean),
    # FWI_max = mean(FWI_max),
    FWI_99th_flag = mean(as.numeric(FWI_99th_flag))-1,
    El_Nino = mean(as.numeric(El_Nino))-1,
    La_Nina = mean(as.numeric(La_Nina))-1,
    Positive_IOD = mean(as.numeric(Positive_IOD))-1,
    Negative_IOD = mean(as.numeric(Negative_IOD))-1,
    Artificial_surfaces = mean(Artificial_surfaces),
    Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
    Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
    Water = mean(Water)) %>% 
  mutate(State = as.factor(State),
         El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))

data_3yr <- data %>% 
  filter(Date > as.Date("2020-06-30")) %>% 
  group_by(State) %>% 
  summarise(# FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag))-1,
            El_Nino = mean(as.numeric(El_Nino))-1,
            La_Nina = mean(as.numeric(La_Nina))-1,
            Positive_IOD = mean(as.numeric(Positive_IOD))-1,
            Negative_IOD = mean(as.numeric(Negative_IOD))-1,
            Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water)) %>% 
  mutate(State = as.factor(State),
         El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))



data_5yr <- data %>% 
  filter(Date > as.Date("2018-06-30")) %>% 
  group_by(State) %>% 
  summarise(# FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag))-1,
            El_Nino = mean(as.numeric(El_Nino))-1,
            La_Nina = mean(as.numeric(La_Nina))-1,
            Positive_IOD = mean(as.numeric(Positive_IOD))-1,
            Negative_IOD = mean(as.numeric(Negative_IOD))-1,
            Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water)) %>% 
  mutate(El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))


data_10yr <- data %>% 
  filter(Date > as.Date("2013-06-30")) %>% 
  group_by(State) %>% 
  summarise(# FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag))-1,
            El_Nino = mean(as.numeric(El_Nino))-1,
            La_Nina = mean(as.numeric(La_Nina))-1,
            Positive_IOD = mean(as.numeric(Positive_IOD))-1,
            Negative_IOD = mean(as.numeric(Negative_IOD))-1,
            Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water)) %>% 
  mutate(El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))



data_full <- data %>% 
  group_by(State) %>% 
  summarise(# FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag))-1,
            El_Nino = mean(as.numeric(El_Nino))-1,
            La_Nina = mean(as.numeric(La_Nina))-1,
            Positive_IOD = mean(as.numeric(Positive_IOD))-1,
            Negative_IOD = mean(as.numeric(Negative_IOD))-1,
            Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water)) %>% 
  mutate(El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))


data_pit <- data %>% 
  filter(Date == as.Date("2023-06-30")) %>% 
  select(-c("Date", "Bushfire_Flag"))

data_weighted <- data %>% 
  group_by(State, FWI_99th_flag, El_Nino, La_Nina, Positive_IOD, Negative_IOD) %>% 
  summarise(Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water),
            Weight = n()/nrow(data)*7)


data_1yr <- rbind(x_train[1,], data_1yr)[-1,]
data_3yr <- rbind(x_train[1,], data_3yr)[-1,]
data_5yr <- rbind(x_train[1,], data_5yr)[-1,]
data_10yr <- rbind(x_train[1,], data_10yr)[-1,]
data_full <- rbind(x_train[1,], data_full)[-1,]
data_pit <- rbind(x_train[1,], data_pit)[-1,]
# data_weighted <- rbind(x_train[1,], data_weighted)[-1,]

pred_1yr <- predict(GBM_best, newdata = data_1yr, type= "prob")
pred_1yr <- as.data.frame(pred_1yr) #convert to df

pred_3yr <- predict(GBM_best, newdata = data_3yr, type= "prob")
pred_3yr <- as.data.frame(pred_3yr) #convert to df

pred_5yr <- predict(GBM_best, newdata = data_5yr, type= "prob")
pred_5yr <- as.data.frame(pred_5yr) #convert to df

pred_10yr <- predict(GBM_best, newdata = data_10yr, type= "prob")
pred_10yr <- as.data.frame(pred_10yr) #convert to df

pred_full <- predict(GBM_best, newdata = data_full, type= "prob")
pred_full <- as.data.frame(pred_full) #convert to df

pred_pit <- predict(GBM_best, newdata = data_pit, type= "prob")
pred_pit <- as.data.frame(pred_pit) #convert to df

pred_weighted <- predict(GBM_best, newdata = data_weighted, type= "prob")
pred_weighted <- as.data.frame(pred_weighted) #convert to df

pred_weighted_summary <- cbind(data_weighted, Probability = pred_weighted$Yes) %>% 
  mutate(Weighted_probability = Weight * Probability) %>% 
  group_by(State) %>% 
  summarise(Prediction = sum(Weighted_probability))



prediction_summary <- data.frame(States = data_full$State,
                                 Pred_1yr = pred_1yr$Yes,
                                 Pred_3yr = pred_3yr$Yes,
                                 Pred_5yr = pred_5yr$Yes,
                                 Pred_10yr = pred_10yr$Yes,
                                 Pred_full = pred_full$Yes,
                                 Pred_pit = pred_pit$Yes,
                                 Pred_weighted = pred_weighted_summary$Prediction)

#
aus_state_shp <- read_sf("./Data/Geospatial Data/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")


bushfire_risk_by_state <- aus_state_shp %>% 
  full_join(prediction_summary, by=c("STE_NAME21" = "States"))

pacman::p_load("dplyr", "rgdal", "ggplot2", "tmap", "ggmap", "sf", "ggspatial", "rlang", "broom", "tidyverse", "raustats", "purr", "readxl", "wesanderson", "lubridate")

bushfire_risk_by_state <- na.omit(bushfire_risk_by_state)

# Heat map
ggplot() +
  geom_sf(data = bushfire_risk_by_state,
          aes(fill = Pred_weighted)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) +
  ggtitle("Bushfire Frequency in Australia (weighted)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_text("Bushfire Risk"))

write.csv(prediction_summary, "./Data/Prediction Summary.csv", row.names = FALSE)
