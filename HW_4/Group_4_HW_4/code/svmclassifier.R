library(caret)
library(class)
set.seed(7)
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



### linear SVM
svm_linear <- train(target~. , data = train_data, method = "svmLinear",
                    trcontrol = trainControl(method = "repeatedcv",number = 10) ,
                    preProcess = c("center","scale"), tuneLength = 10 )

svm_linear

test_pred <- predict(svm_linear,test_data)

### confusionmatrix
confusionMatrix(table(test_pred,test_label))

### best c value

svm_grid <- train(target~. , data = train_data, method = "svmLinear",
                  trcontrol = trainControl(method = "cv",number = 10) ,
                  preProcess = c("center","scale"), 
                  tuneGrid = expand.grid(C = seq(0,2,length = 20)) )

plot(svm_grid)
svm_grid$bestTune
test_predgrid <- predict(svm_grid,test_data)
confusionMatrix(table(test_predgrid,test_label))

### prob of mis-classification
mis_prob <- 32/(107+32) + 16/(153 + 16)
mis_prob



### for breast cancer
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



### linear SVM
svm_linear <- train(diagnosis~. , data = train_data, method = "svmLinear",
                    trcontrol = trainControl(method = "repeatedcv",number = 10) ,
                    preProcess = c("center","scale"), tuneLength = 10 )

svm_linear

test_pred <- predict(svm_linear,test_data)

### confusionmatrix
confusionMatrix(table(test_pred,test_label))

### best c value

svm_grid <- train(diagnosis~. , data = train_data, method = "svmLinear",
                  trcontrol = trainControl(method = "cv",number = 10) ,
                  preProcess = c("center","scale"), 
                  tuneGrid = expand.grid(C = seq(0,2,length = 20)) )

plot(svm_grid)
svm_grid$bestTune
test_predgrid <- predict(svm_grid,test_data)
confusionMatrix(table(test_predgrid,test_label))


## prob of mis classification
miss_prob <- 7/(56+7)
miss_prob
