#Algorithm: Generative Models: Naive Bayers
#Author: Dayane Gonçalves

######################################################################################################
#Case: Apply nayve bayers to predicting sex from height.
#######################################################################################################
#Step 1:Getting dataset
library(tidyverse)
library(caret)
library(dslabs)
data("heights")

#step 2: Generating training and testing set
y <- heights$height
set.seed(1995)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights |> slice(-test_index)
test_set <- heights |> slice(test_index)

#the Naive Bayes approach is particularly appropriate because we know that the normal distribution
#is a good approximation for the conditional distributions of height given sex for both classes

#step 3:Approximating the conditional distributions
params <- train_set |>
  group_by(sex) |> 
  summarize(avg = mean(height), sd = sd(height))
params
#estimating the prevalence
pi <- train_set |> summarize(pi=mean(sex=="Female")) |> pull(pi)
pi

# Getting an actual rule
x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

qplot(x, p_hat_bayes, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_vline(xintercept = 67, lty = 2)

#Controlling prevalence
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Female", "Male")
sensitivity(factor(y_hat_bayes_unbiased), factor(test_set$sex))
specificity(factor(y_hat_bayes_unbiased), factor(test_set$sex))

qplot(x, p_hat_bayes_unbiased, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_vline(xintercept = 67, lty = 2)

