
#Algorithm: Knn and Randon forest 
#Author: Dayane Gonçalves

####################################################################################################################
#Case: Classify if a digit is one, two, three, four, five, six, seven, eight or nine using KNN and randon forest
####################################################################################################################

#Loading libraries

library(tidyverse)
library(matrixStats)
pacalibrary(dslabs)
library(caret)
library(randomForest)

#Getting Dataset
mnist <- read_mnist()

#Undertanding the dataset

names(mnist) #The dataset includes two components, a training set and test set
#Each of these components includes a matrix with features in the columns
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)
# choosing a subset for feasible time processing purposes:
# 10,000 random rows from the training set
#1,000 random rows from the test set
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

# Preprocessing
sds <- colSds(x)  #compute the standard deviations of columns
qplot(sds, bins = 256) 

# there are several features that do not vary much from observation to observation.
#This is expected because there are parts of the image that rarely contain writing (dark pixels).

nzv <- nearZeroVar(x) #collomns that should be remmoved from the data - there is no info about the number, only white spots
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv) #collumns that shouldn't be removed
length(col_index)

#adding column names to the feature matrices as these are required by caret
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# Predict Algorithm - k-nearest neighbor
n <- 5000
b <- 4
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[, col_index], y, k = 3) #obtimazing knn

y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class") #predicting the result
cm <- confusionMatrix(y_hat_knn, factor(y_test)) #getting the confusion matrix
cm$overall["Accuracy"] 
cm$byClass[,1:2]

#Trying to improve the results using random Forest
#Optimizing the algorithm
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <- train(x[, col_index], y,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune
fit_rf <- randomForest(x[, col_index], y,
                       minNode = train_rf$bestTune$mtry)
plot(fit_rf)

y_hat_rf <- predict(fit_rf, x_test[ ,col_index]) #predicting the result
cm <- confusionMatrix(y_hat_rf, y_test) #getting the confusion matrix
cm$overall["Accuracy"]
cm$byClass[,1:2]
