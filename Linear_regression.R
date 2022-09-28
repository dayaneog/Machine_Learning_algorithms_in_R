#Algorithm: linear regression
#Author: Dayane Gonçalves

######################################################################################################
#Case: building a machine learning algorithm that predicts the son’s height Y using the father’s height X.
#######################################################################################################

#step 1: Getting the data
library(tidyverse)
library(HistData)
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#step 2: Generating training and testing set
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#step 3: getting the linear fit using training data
fit <- lm(son ~ father, data = train_set)
fit$coef 
#linear regression plot
train_set%>%ggplot(aes(father,son))+
geom_point() +
geom_abline(intercept = fit$coef[1], slope = fit$coef[2],color = "red")

#step 4: estimating the results using linear regression
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2) # squared loss result

#getting the linear regression result using the predict function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2) # squared loss result

