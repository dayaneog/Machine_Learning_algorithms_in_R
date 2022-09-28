#Algorithm: Logistic regression
#Author: Dayane Gonçalves

######################################################################################################
#Case: Apply logistic regression to classify whether a digit is two or seven
#######################################################################################################

#Step 1: Loading libraries
library(tidyverse)
library(dslabs)
library(caret)
#Step 1: Getting Dataset
data("mnist_27") 

#dataset parameters
#x1<- the proportion of dark pixels that are in the upper left quadrant
# x2<- the proportion of dark pixels that are in the lower right quadrant 
#training dataset <- 500 samples
#testing dataset <- 500 samples
head(mnist_27$train)
mnist_27$train  %>% ggplot(aes(x_1, x_2, color = y)) + geom_point() #plotting training dataset


#Step 2: fitting the train data using a logistic regression
fit <- mnist_27$train |>
  mutate(y = ifelse(y==7, 1, 0)) |>
  lm(y ~ x_1 + x_2, data = _)

#Step 3: Predicting the result using the test dataset
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]] #getting the model's accuracy 


# Plot of the true conditional distribution
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")
