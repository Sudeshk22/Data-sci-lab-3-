### depth based classifier
library(ddalpha)
library(readr)
heart <- read_csv("heart.csv")
data <- heart
combind <- data[,-14]
class1 <- data$target == "0"
x1 <- data[class1,]
x1 <- x1[,-14]

class2 <- data$target == "1"
x2 <- data[class2,]
x2 <- x2[,-14]
### Depth value wrt first group
depth.wrt.class1 <- depth.Mahalanobis(combind,x1)
### Depth value wrt Second group
depth.wrt.class2 <- depth.Mahalanobis(combind,x2)



target <- data$target
pred_target <- numeric(length = nrow(data))
for (i in 1:nrow(data)) {
  
  if(depth.wrt.class1[i] <= depth.wrt.class2[i]){
    pred_target[i] <- 1
  }
}
tab <- cbind(target,pred_target)
print(tab)
### miss classification probablity

count <- 0
for (i in 1:1025) {
  if (target[i] != pred_target[i]){
    count <- count + 1
  }
}


Miss_prob <- count/1025
Miss_prob



#### for breast cancer data
library(readr)
breast_cancer <- read_csv("breast-cancer.csv")
data <- breast_cancer 
data <- data[,-1]
combinddata <- data[,-1]

## partition
grop1 <- data$diagnosis == "M"
gr1 <- data[grop1,]
gr1 <- gr1[,-1]



grop2 <- data$diagnosis == "B"
gr2 <- data[grop2,]
gr2 <- gr2[,-1]

### Depth value wrt first group M
depth.wrt.class1 <- depth.Mahalanobis(combinddata,gr1)
### Depth value wrt Second group B
depth.wrt.class2 <- depth.Mahalanobis(combinddata,gr2)


diagnosis <- data$diagnosis

pred_diagnosis <- character(length = nrow(combinddata))
for (i in 1:nrow(data)) {
  
  if(depth.wrt.class1[i] > depth.wrt.class2[i]){
    pred_diagnosis[i] <- "M"
  }else{
    pred_diagnosis[i] <- "B"
  }
}
tab <- cbind(diagnosis,pred_diagnosis)
print(tab)
### miss classification probablity

count <- 0
for (i in 1:nrow(combinddata)) {
  if (diagnosis[i] != pred_diagnosis[i]){
    count <- count + 1
  }
}


Miss_prob <- count/1025
Miss_prob
