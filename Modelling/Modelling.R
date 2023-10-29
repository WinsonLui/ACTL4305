
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
#install.packages("pacman")
pacman::p_load("readxl","dplyr", "tidyr", "lubridate", "ggplot2", "hrbrthemes", "ggmap", "broom", "geojsonio", "maptools", "sf", "glmnet", "parallel", "doParallel", "PRROC", "caret")

# Load data 
data <- read.table("./Data/Cleaned Data/df.csv", sep=",", header=TRUE)


# Process data
data <- data %>% 
  mutate(El_Nino = (SOI > 7)) %>% 
  mutate(La_Nina = (SOI < -7)) %>% 
  mutate(Positive_IOD = (IOD > 0.4)) %>% 
  mutate(Negative_IOD = (IOD < -0.4)) %>% 
  select(-c("FWI_median", "FWI_max", "FWI_99th", "IOD", "SOI"))

data$Date <- ymd(data$Date)
data$State <- as.factor(data$State)
data$FWI_99th_flag <- as.factor(data$FWI_99th_flag)
data$Bushfire_Flag <- as.factor(data$Bushfire_Flag)
data$El_Nino <- as.factor(data$El_Nino)
data$La_Nina <- as.factor(data$La_Nina)
data$Positive_IOD <- as.factor(data$Positive_IOD)
data$Negative_IOD <- as.factor(data$Negative_IOD)

# Prepare Data
set.seed(6)
index <- sample(1:nrow(data), 0.7*nrow(data))

x_train <- data[index, -9][,-1]
y_train <- data[index, 9]
data_train <- data[index,][,-1]

x_test <- data[-index, -9][,-1]
y_test <- data[-index, 9]
data_test <- data[-index,][,-1]

x_train_matrix <- model.matrix(~., x_train)
y_train_matrix <- as.matrix(y_train)

x_test_matrix <- model.matrix(~., x_test)
y_test_matrix <- as.matrix(y_test)

# 1. GLM Models
## 1(1)
## 1A. Full GLM Model 
LogisticModel <- glm(formula = Bushfire_Flag ~ State + FWI_mean + FWI_99th_flag + El_Nino + La_Nina + Positive_IOD + Negative_IOD + Artificial_surfaces + Cultivated_terrestrial_vegetated + Natural_terrestrial_vegetated + Water,
                data = data,
                family = "binomial")

summary(LogisticModel)

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
# 1B2. Ridge penalty
CV_ridge <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                      alpha = 0, nfolds = 10, parallel = T)
# 1B3. Elasticnet penalty
CV_EN <- cv.glmnet(x_train_matrix, y_train_matrix, family="binomial", type.measure = "auc",
                   alpha = 0.5, nfolds = 10, parallel = T)

stopCluster(cl) # ending parallel computing - important, otherwise R can get funky.

### Visualisation between regularisation parameter (lambda) and model coefficient
plot(CV_lasso$glmnet.fit, xvar = "lambda",main="Lasso")
plot(CV_ridge$glmnet.fit, xvar = "lambda",main="Ridge")
plot(CV_EN$glmnet.fit,    xvar = "lambda",main="Elasticnet")

# 1.Make predictions
prediction_lasso <- predict(CV_lasso, s=CV_lasso$lambda.min, newx=x_test_matrix, type="response")
prediction_ridge <- predict(CV_ridge, s=CV_ridge$lambda.min, newx=x_test_matrix, type="response")
prediction_EN <- predict(CV_EN, s=CV_EN$lambda.min, newx=x_test_matrix, type="response")
prediction_Logistic <- predict(LogisticModel, newdata=data_test, type="response")

# ROC curve
ROC_lasso<-PRROC::roc.curve(scores.class0 = prediction_lasso,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_ridge<-PRROC::roc.curve(scores.class0 = prediction_ridge,
                            weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_EN<-PRROC::roc.curve(scores.class0 = prediction_EN,
                         weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)

ROC_Logistic<-PRROC::roc.curve(scores.class0 = prediction_Logistic,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)



# ROC
ROC_lasso$auc #Lasso
ROC_ridge$auc #Ridge
ROC_EN$auc #EN
ROC_Logistic$auc #simple logistic 

# Confusion matrix
# PredClass_lasso<- as.integer(prediction_lasso>0.5)
# PredClass_ridge<- as.integer(prediction_ridge>0.5)
# PredClass_EN<- as.integer(prediction_EN>0.5)
# PredClass_Logistic<- as.integer(prediction_Logistic>0.5)
# 
# ConfuMatrix_lasso<- confusionMatrix(as.factor(PredClass_lasso), data_test$Bushfire_Flag, positive="1")
# ConfuMatrix_ridge<- confusionMatrix(as.factor(PredClass_ridge), data_test$Bushfire_Flag, positive="1")
# ConfuMatrix_EN<- confusionMatrix(as.factor(PredClass_EN), data_test$Bushfire_Flag, positive="1")
# ConfuMatrix_Logistic<- confusionMatrix(as.factor(PredClass_Logistic), data_test$Bushfire_Flag, positive="1")


# predict(CV_lasso, s=CV_lasso$lambda.min, newx=x_test_matrix, type="response")


# 2. Tree-based Models

# Look for best mtry
# bestmtry <- tuneRF(x_train, y_train, ntreeTry = 500,
#                   mtryStart  = 5,stepFactor = 1.2, improve = 0.01, trace=T)

bestmtry <- 6
#14 using old data, 8 using new data

# Create RF model
RF <- randomForest(Bushfire_Flag~.,data= data_train, mtry=bestmtry)
RF #summary
importance(RF) # shows how much each factor contributes to GINI index (error) decrease

# Prediction
prediction_RF <- predict(RF, newdata = x_test, type="prob")
prediction_RF <- as.data.frame(prediction_RF) #convert to df
# colnames(pred_test)[1] = "prediction" #rename column

# Re-order soft delete (no need to reorder since both datasets are in the same jumbled up order)
#pred_test$index <- as.numeric(row.names(pred_test))
#df <- as.data.frame(pred_test[order(pred_test$index), ]$prediction)
#colnames(df)[1] = "prediction"

# ROC
ROC_RF<-PRROC::roc.curve(scores.class0 = prediction_RF$`TRUE`,
                               weights.class0 = as.numeric(data_test$Bushfire_Flag)-1,curve = T)
ROC_RF$auc # summary and AUC


plot(ROC_lasso,color = "brown", main="ROC curves", auc.main = F, lwd=2)
plot(ROC_ridge,color ="blue",add=T, lwd=2)
plot(ROC_EN,color ="red",add=T, lwd=2)
plot(ROC_Logistic,color ="yellow",add=T, lwd=2)
plot(ROC_RF,add=T,color ="black", lwd=2)
legend("bottomright", legend = c("Lasoo", "Ridge","EN", "Simple logistic", "Random forest"),
       lwd = 3, col = c("brown", "blue","red", "yellow", "black"))

#
data_3yr <- data %>% 
  filter(Date > as.Date("2020-06-30")) %>% 
  group_by(State) %>% 
  summarise(FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag)),
            El_Nino = mean(as.numeric(El_Nino)),
            La_Nina = mean(as.numeric(La_Nina)),
            Positive_IOD = mean(as.numeric(Positive_IOD)),
            Negative_IOD = mean(as.numeric(Negative_IOD)),
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
  summarise(FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag)),
            El_Nino = mean(as.numeric(El_Nino)),
            La_Nina = mean(as.numeric(La_Nina)),
            Positive_IOD = mean(as.numeric(Positive_IOD)),
            Negative_IOD = mean(as.numeric(Negative_IOD)),
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
  summarise(FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag)),
            El_Nino = mean(as.numeric(El_Nino)),
            La_Nina = mean(as.numeric(La_Nina)),
            Positive_IOD = mean(as.numeric(Positive_IOD)),
            Negative_IOD = mean(as.numeric(Negative_IOD)),
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
  summarise(FWI_mean = mean(FWI_mean),
            # FWI_max = mean(FWI_max),
            FWI_99th_flag = mean(as.numeric(FWI_99th_flag)),
            El_Nino = mean(as.numeric(El_Nino)),
            La_Nina = mean(as.numeric(La_Nina)),
            Positive_IOD = mean(as.numeric(Positive_IOD)),
            Negative_IOD = mean(as.numeric(Negative_IOD)),
            Artificial_surfaces = mean(Artificial_surfaces),
            Cultivated_terrestrial_vegetated = mean(Cultivated_terrestrial_vegetated),
            Natural_terrestrial_vegetated = mean(Natural_terrestrial_vegetated),
            Water = mean(Water)) %>% 
  mutate(El_Nino = as.factor(El_Nino>0.5),
         La_Nina = as.factor(La_Nina>0.5),
         Positive_IOD = as.factor(Positive_IOD>0.5),
         Negative_IOD = as.factor(Negative_IOD>0.5),
         FWI_99th_flag = as.factor(FWI_99th_flag>0.5))

data_3yr <- rbind(x_train[1,], data_3yr)[-1,]
data_5yr <- rbind(x_train[1,], data_5yr)[-1,]
data_10yr <- rbind(x_train[1,], data_10yr)[-1,]
data_full <- rbind(x_train[1,], data_full)[-1,]

pred_3yr <- predict(RF, newdata = data_3yr, type= "prob")
pred_3yr <- as.data.frame(pred_3yr) #convert to df

pred_5yr <- predict(RF, newdata = data_5yr, type= "prob")
pred_5yr <- as.data.frame(pred_5yr) #convert to df

pred_10yr <- predict(RF, newdata = data_10yr, type= "prob")
pred_10yr <- as.data.frame(pred_10yr) #convert to df

pred_full <- predict(RF, newdata = data_full, type= "prob")
pred_full <- as.data.frame(pred_full) #convert to df

prediction_summary <- data.frame(States = data_full$State,
                                 Pred_3yr = pred_3yr$`TRUE`,
                                 Pred_5yr = pred_5yr$`TRUE`,
                                 Pred_10yr = pred_10yr$`TRUE`,
                                 Pred_full = pred_full$`TRUE`)


