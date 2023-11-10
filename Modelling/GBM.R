#### Gradient Boosting Machine
# tuning parameters
# parameters <- expand.grid(n.trees =  (1:20)*50,
#                           interaction.depth = c(1, 2, 3),
#                           shrinkage = c(0.01, 0.1),
#                           n.minobsinnode = 10)
parameters <- data.frame(n.trees =  1000,
                         interaction.depth = 3,
                         shrinkage = 0.1,
                         n.minobsinnode = 10)

GBMModel <- train(Bushfire_Flag ~.,
                  data=data_train, method="gbm", distribution = "bernoulli", 
                  metric = "ROC", trControl = fitcontrol, tuneGrid = parameters, verbose = FALSE)

# plot(GBMModel)
# GBMModel$bestTune # find optimal tuning parameters and rerun model

### Feature Importance
plot(varImp(GBMModel), main = "Gini Impurity Improvement") # Gini impurity improvement

par(mar = c(5, 13, 1, 1))
#summary(GBMModel, cBars = 15, method = relative.influence, las = 2, main="Gini Impurity Improvement") # Gini impurity improvement
summary(GBMModel, cBars = 15, method = permutation.test.gbm, las = 2, main="OOB Prediction Accuracy Improvement") # prediction accuracy using permutation

### Partial Dependence Plot
par(mar = c(5, 4, 4, 2))
partial(GBMModel, pred.var = "State", plot = TRUE, prob = TRUE,
        n.trees = GBMModel$n.tree, plot.engine = "ggplot2", lwd = 2)

### ROC
prob_GBMModel <- predict(GBMModel, newdata = x_test, type = "prob")
ROC_GBMModel <- roc.curve(scores.class0 = prob_GBMModel$Yes, 
                          weights.class0 = as.numeric(y_test)-1, curve = T)

### Confusion Matrix
prediction_GBMModel <- predict(GBMModel, newdata = x_test, type = "raw")
GBMModel_conf <- confusionMatrix(prediction_GBMModel, y_test, positive="Yes")