#### K-nn classifier
### reading the data
library(caret)
library(class)

data <- breast_cancer 
data <- data[,-1]
data$diagnosis <- as.factor(data$diagnosis)
str(data)


### partition
train_sample <- sample(1:nrow(data), nrow(data)*0.7)
train_data <- data[train_sample,]
test_data <- data[-train_sample,]
train_label <- data[train_sample,1]
test_label <- data[-train_sample,1]
test_label <- as.factor(test_label$diagnosis)


trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

## initiak k
set.seed(7)
fit.knn1 <- train(diagnosis~., data=train_data, method="knn",
                 metric=metric ,trControl=trainControl)
knn.k1 <- fit.knn1$bestTune # keep this Initial k for testing with knn() function in next section
print(fit.knn1)

### plot of k
plot(fit.knn1)


### prediction
set.seed(7)
prediction <- predict(fit.knn1,test_data)
cf <- confusionMatrix(prediction, test_label)
print(cf)

 ### grid search for best K value
set.seed(7)
grid <- expand.grid(.k=seq(1,20,by=1))
fit.knn2 <- train(diagnosis~., data=train_data, method="knn", 
                 metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn2$bestTune # keep this optimal k for testing with stand alone knn() function in next section
print(fit.knn2)


plot(fit.knn2)

### prediction
set.seed(7)
prediction <- predict(fit.knn2,test_data)
cf <- confusionMatrix(prediction, test_label)
print(cf)


### miss-classification probability for knn k = 17 classifier
prob_mis <- 5/100 + 8/71
prob_mis








### for heart data


#### K-nn classifier
### reading the data
library(caret)
library(class)

data <- heart
data$target <- as.factor(data$target)
str(data)


### partition
train_sample <- sample(1:nrow(data), nrow(data)*0.7)
train_data <- data[train_sample,]
test_data <- data[-train_sample,]
train_label <- data[train_sample,14]
test_label <- data[-train_sample,14]
test_label <- as.factor(test_label$target)


trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

## initiak k
set.seed(7)
fit.knn1 <- train(target~., data=train_data, method="knn",
                  metric=metric ,trControl=trainControl)
knn.k1 <- fit.knn1$bestTune # keep this Initial k for testing with knn() function in next section
print(fit.knn1)

### plot of k
plot(fit.knn1)


### prediction
set.seed(7)
prediction <- predict(fit.knn1,test_data)
cf <- confusionMatrix(prediction, test_label)
print(cf)

### grid search for best K value
set.seed(7)
grid <- expand.grid(.k=seq(1,20,by=1))
fit.knn2 <- train(target~., data=train_data, method="knn", 
                  metric=metric, tuneGrid=grid, trControl=trainControl)
knn.k2 <- fit.knn2$bestTune # keep this optimal k for testing with stand alone knn() function in next section
print(fit.knn2)


plot(fit.knn2)

### prediction
set.seed(7)
prediction <- predict(fit.knn2,test_data)
cf <- confusionMatrix(prediction, test_label)
print(cf)


### miss-classification probability for knn k = 17 classifier
prob_mis <- 16/(157 +16)
prob_mis
