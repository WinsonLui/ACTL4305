## Set working directory
setwd("C:/Users/Callista Surjadi/Downloads")

## Initialise libraries
pacman::p_load("readxl","dplyr", "randomForest", "gridExtra", "tidyr", "lubridate", "ggplot2", "hrbrthemes", "ggmap", "broom", "geojsonio", "maptools", "sf", "glmnet", "parallel", "doParallel", "PRROC", "caret")

## Import dataset
data <- read.csv("df.csv")

## Process data
data <- data %>%
  mutate(SOI_Condition = case_when(SOI <= 7 & SOI >= -7 ~ "Normal SOI",
                                   SOI > 7 ~ "El Nino",
                                   SOI < -7 ~ "La Nina")) %>%
  mutate(IOD_Phase = case_when(IOD <= 0.4 & IOD >= -0.4 ~ "Normal IOD",
                               IOD > 0.4 ~ "Positive",
                               IOD < -0.4 ~ "Negative")) %>% 
  mutate(FWI_99th_flag = case_when(FWI_max < FWI_99th ~ "No",
                                   FWI_max >= FWI_99th ~ "Yes")) %>%
  select(-c("FWI_median", "FWI_max", "FWI_99th", "IOD", "SOI"))

data$Bushfire_Flag <- ifelse(data$Bushfire_Flag == T, "Yes", "No")

data <- data %>%
  select("Bushfire_Flag", everything())

data$Date <- ymd(data$Date)
data$State <- as.factor(data$State)
data$FWI_99th_flag <- as.factor(data$FWI_99th_flag)
data$Bushfire_Flag <- as.factor(data$Bushfire_Flag)
data$SOI_Condition <- as.factor(data$SOI_Condition)
data$IOD_Phase <- as.factor(data$IOD_Phase)

## Split data to training and testing set
set.seed(6)
index <- sample(1:nrow(data), 0.7*nrow(data))

x_train <- data[index, -1]
y_train <- data[index, 1]
data_train <- data[index,]

x_test <- data[-index, -1]
y_test <- data[-index, 1]
data_test <- data[-index,]

x_train_matrix <- model.matrix(~., x_train)
y_train_matrix <- as.matrix(y_train)

x_test_matrix <- model.matrix(~., x_test)
y_test_matrix <- as.matrix(y_test)

## GLM
### Full model
LogisticModel <- glm(formula = Bushfire_Flag ~ State + FWI_99th_flag + SOI_Condition + IOD_Phase + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water,
                     data = data,
                     family = "binomial")

summary(LogisticModel)

### Shrinkage models -> find tuning parameter for feature selection
# - alpha=1 is the lasso penalty,
# - alpha=0 the ridge penalty,
# - alpha=0.5 the elasticnet penalty,

# Starting parallel computing
cl <- makeCluster(detectCores()-1) # this detects the cores of your computer

registerDoParallel(cl) # start the parallel computing mode to speed up the CV process.

#### Lasso penalty
CV_lasso <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                      alpha = 1, nfolds = 10, parallel = T)
#### Ridge penalty
CV_ridge <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                      alpha = 0, nfolds = 10, parallel = T)
#### Elasticnet penalty
CV_EN <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                   alpha = 0.5, nfolds = 10, parallel = T)

stopCluster(cl) # ending parallel computing - important, otherwise R can get funky.

### Compare regularisation parameter (lambda) and model coefficient
plot(CV_lasso$glmnet.fit, xvar = "lambda",main="Lasso")
plot(CV_ridge$glmnet.fit, xvar = "lambda",main="Ridge")
plot(CV_EN$glmnet.fit,    xvar = "lambda",main="Elasticnet")

### Model predictions
prediction_lasso <- predict(CV_lasso, s=CV_lasso$lambda.min, newx=x_test_matrix, type="response")
prediction_ridge <- predict(CV_ridge, s=CV_ridge$lambda.min, newx=x_test_matrix, type="response")
prediction_EN <- predict(CV_EN, s=CV_EN$lambda.min, newx=x_test_matrix, type="response")
prediction_Logistic <- predict(LogisticModel, newdata=data_test, type="response")

### Test error: ROC
ROC_lasso<-PRROC::roc.curve(scores.class0 = prediction_lasso,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_ridge<-PRROC::roc.curve(scores.class0 = prediction_ridge,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_EN<-PRROC::roc.curve(scores.class0 = prediction_EN,
                         weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Logistic<-PRROC::roc.curve(scores.class0 = prediction_Logistic,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

plot(ROC_lasso,color = "brown", main="ROC curves", auc.main = F, lwd=2)
plot(ROC_ridge,color ="blue",add=T, lwd=2)
plot(ROC_EN,color ="red",add=T, lwd=2)
plot(ROC_Logistic,color ="grey",add=T, lwd=2)
legend("bottomright", legend = c("Lasoo", "Ridge","EN", "Simple logistic"),
       lwd = 3, col = c("brown", "blue","red", "grey"))

## Tree-based models
### Define control parameters for train
fitcontrol <- trainControl(method = "cv", 
                           number = 5,
                           savePredictions = TRUE,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)

set.seed(319)

### Train data
# Starting parallel computing
cl <- makeCluster(detectCores()-1) # this detects the cores of your computer

registerDoParallel(cl) # start the parallel computing mode to speed up the training process.

#### Classification and Regression Tree
CARTModel <- train(Bushfire_Flag ~ State + FWI_99th_flag + SOI_Condition + IOD_Phase + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water, data = data_train, method = "rpart", metric = "ROC",
               trControl = fitcontrol, tuneLength = 10)

#### Bagging
tuning_grid <- expand.grid(
  maxdepth = c(1, 3, 5, 8, 15),
  minsplit = c(2, 5, 10, 15),
  ROC = NA,
  Sens = NA,
  Spec = NA
)

set.seed(761)

for(i in seq_len(nrow(tuning_grid))) {
  
  # Fit model for each hyperparameter combination
  fit <- train(Bushfire_Flag ~ State + FWI_99th_flag + SOI_Condition + IOD_Phase + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water, data = data_train, method = "treebag", metric = "ROC",
               trControl = fitcontrol,
               maxdepth = tuning_grid$maxdepth[i],
               minsplit = tuning_grid$minsplit[i])
  
  
  # Save fit metrics from each model...
  tuning_grid$ROC[i] <- fit$results$ROC
  tuning_grid$Sens[i] <- fit$results$Sens
  tuning_grid$Spec[i] <- fit$results$Spec
}

# Assess top 10 models
tuning_grid %>%
  arrange(-ROC) %>%
  head(10)

best_ROC <- tuning_grid[tuning_grid$ROC == max(tuning_grid$ROC), ]

# Re-run model with best hyperparameters
BagModel <- train(Bushfire_Flag ~ State + FWI_99th_flag + SOI_Condition + IOD_Phase + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water, data = data_train, method = "treebag", metric = "ROC",
             trControl = fitcontrol,
             maxdepth = best_ROC$maxdepth,
             minsplit = best_ROC$minsplit)

#### Random Forest
RandomForestModel <- train(Bushfire_Flag ~ State + FWI_99th_flag + SOI_Condition + IOD_Phase + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water, data = data_train, method = "rf", metric = "ROC",
                           trControl = fitcontrol, tuneGrid = expand.grid(mtry = seq(1,15,1)))

stopCluster(cl) # ending parallel computing - important, otherwise R can get funky.

### Compare number of trees and error rates
plot(CARTModel)
#plot(BagModel) # no tuning parameters
plot(RandomForestModel)

### print results
print(CARTModel)
print(BagModel)
print(RandomForestModel)

### Model predictions
#Note the threshold probability for the classification is 50% by default.
prob_CARTModel <- predict(CARTModel, newdata = x_test, type = "prob") 
prob_BagModel <- predict(BagModel, newdata = x_test, type = "prob")
prob_RandomForestModel <- predict(RandomForestModel, newdata = x_test, type = "prob")

### Confusion Matrix
prediction_CARTModel <- predict(CARTModel, newdata = x_test, type = "raw") 
prediction_BagModel <- predict(BagModel, newdata = x_test, type = "raw")
prediction_RandomForestModel <- predict(RandomForestModel, newdata = x_test, type = "raw")

CARTModel_conf <- confusionMatrix(prediction_CARTModel,  y_test, positive="Yes")
BagModel_conf <- confusionMatrix(prediction_BagModel,  y_test, positive="Yes")
RandomForestModel_conf <- confusionMatrix(prediction_RandomForestModel,  y_test, positive="Yes")

CARTModel_conf$table
BagModel_conf$table
RandomForestModel_conf$table

### Test error: ROC
ROC_CARTModel <- roc.curve(scores.class0 = prob_CARTModel$Yes, 
                       weights.class0 = as.numeric(data_test$Bushfire_Flag)-1, curve = T)

ROC_BagModel <- roc.curve(scores.class0 = prob_BagModel$Yes, 
                          weights.class0 = as.numeric(data_test$Bushfire_Flag)-1, curve = T)

ROC_RandomForestModel <- roc.curve(scores.class0 = prob_RandomForestModel$Yes, 
                          weights.class0 = as.numeric(data_test$Bushfire_Flag)-1, curve = T)

plot(ROC_CARTModel,color = "brown", main="ROC curves", auc.main = F, lwd=2)
plot(ROC_BagModel,color ="blue",add=T, lwd=2)
plot(ROC_RandomForestModel,color ="red",add=T, lwd=2)
legend("bottomright", legend = c("CART", "Bagging","Random Forest"),
       lwd = 3, col = c("brown", "blue","red"))

