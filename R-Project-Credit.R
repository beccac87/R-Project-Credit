#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements!
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ISLR2)

#Question 1
#Use the data function to load Credit into R.  Credit is a dataset containing information on 400 credit card customers.  The variables include:
data(Credit)

head(Credit)
dim(Credit)

#Question 2
#Next, create training and testing data sets.  Your code should do the following:
#Set a seed equal to 13215 exactly like we did in class, using “Mersenne-Twister” as the algorithm and “Rejection” as the sample.kind.
#Draw a random sample of rows equal in length to 60% of the number of rows in your data. 
#Create a training data set with the rows in your random sample.
#Create a testing data set with the rows not in your random sample.
set.seed(13215, "Mersenne-Twister", sample.kind = "Rejection")

rows <- sample(nrow(Credit), 0.6*nrow(Credit))

training_data <- Credit[rows,]
test_data <- Credit[-rows,]

head(rows, 5)
dim(training_data)
dim(test_data)

#Question 3
#Next, estimate a linear regression with your training data set and Limit as the outcome variable (y-variable) and the following explanatory variables:  Income, Rating, Age, Cards, Education, Own, Student, Married, and Region. 
reg_1 <- lm(Limit ~ Income + Rating  +
                      Age + Cards + Education +
                      Own + Student + Married + Region, 
                    data = training_data)

round(reg_1$coeff, digits = 2)
summary(reg_1)

#Question 4
#Use your testing data and linear regression to predict credit limit.  What is the RMSE of these predictions?  Round to three decimal places.
test_data <- test_data %>%
  mutate(reg_1_predictions = predict(reg_1, test_data)) %>%
  mutate(reg_1_error = Limit - reg_1_predictions)

round(sqrt(mean((test_data$reg_1_error)^2)), digits = 3)

#Question 5
#Estimate a regression tree with your training data set and Limit as the outcome variable (y-variable) and the following explanatory variables:  Income, Rating, Age, Cards, Education, Own, Student, Married, and Region. 
reg_tree_1 <- rpart(Limit ~ Income + Rating  + Age +
                      Cards + Education + Own +
                      Student + Married + Region, 
                    data = training_data, method = "anova")

print(reg_tree_1)
rpart.plot(reg_tree_1)

#Question 6
#Use your testing data and regression tree to predict credit limit. 
test_data <- test_data %>%
  mutate(reg_tree_1_predictions = predict(reg_tree_1, test_data)) %>%
  mutate(reg_tree_1_error = Limit - reg_tree_1_predictions)

length(unique(test_data$reg_tree_1_predictions))
round(sqrt(mean((test_data$reg_tree_1_error)^2)), digits = 3)

#Question 7: S/A

#Question 8
#Estimate a forest of regression trees with your training data set and Limit as the outcome variable (y-variable) and the following explanatory variables:  Income, Rating, Age, Cards, Education, Own, Student, Married, and Region.  Immediately before running your forest, set a seed of 13215. 
set.seed(13215, "Mersenne-Twister", sample.kind = "Rejection")

rf <- randomForest(Limit ~ Income + Rating + Age + Cards + 
                     Education + Own + 
                     Student + Married + Region, 
                   data = training_data, proximity = TRUE)

print(rf)
print(importance(rf,type = 2)) 
varImpPlot(rf)

#Question 9
#Use your testing data and forest of regression trees to predict credit limit. 
test_data <- test_data %>%
  mutate(forest_1_predictions = predict(rf, test_data)) %>%
  mutate(forest_1_error = Limit - forest_1_predictions)

length(unique(test_data$forest_1_predictions))
round(sqrt(mean((test_data$forest_1_error)^2)), digits = 3)

#Question 10
#Use ggplot to plot your predicted credit limit (y-variable) against your actual credit limit (x-variable) in your test data.  Label your axes appropriately.  Save your figure as a pdf, 6 inches by 6 inches in dimension.  Upload your graph below.
plot_1 <- ggplot(data = test_data, aes(x = Limit, y = reg_tree_1_predictions)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Limit",
       y = "Predicted Limit", 
       title = "Predicted Limits using a regression tree")

ggsave(filename = "Desktop/plot_1.pdf",
       plot = plot_1,
       height = 6,
       width = 6,
       units = c("in"))

#Question 11: S/A

#Question 12
#Now, we will switch to examining a binary outcome variable.  Use the data function to load Default into R.  Credit is a dataset containing information on 10,000 credit card customers.  The variables include:
data(Default)

head(Default)
dim(Default)

#Question 13
#The class of the variable we want to use as our outcome variable, default, is factor, so create a binary variable equal to 1 if default equals “Yes”.  What is the average of that binary variable?  Round your solution to three decimal places.
Default <- Default %>%
  mutate(default_binary = ifelse(default == "Yes", 1, 0))

round(mean(Default$default_binary), digits = 3)

#Question 14
#Calculate the following answers, do not round intermediate solutions, and round all final solutions to three decimal places.
prob_default_binary <- mean(Default$default_binary)
prob_not_default_binary <- 1 - prob_default_binary

entropy_whole_sample <- -1* (prob_default_binary*log(prob_default_binary,base = 2) + 
                               prob_not_default_binary*log(prob_not_default_binary, base = 2))

round(prob_default_binary, digits = 3)
round(entropy_whole_sample, digits = 3)

#Question 15
#Based on in-class discussions, you do not believe that the student variable is going to help in your analysis, but you decide to do some calculations before deciding to focus on other explanatory variables, to determine whether student is relevant to predicting default.  So, you create a sample of student borrowers and non-student borrowers and calculate entropy for both samples, and a weighted average of the two entropy numbers.  Calculate the following answers, do not round intermediate solutions, and round all final solutions to three decimal places.
##student:
Default_student <- Default %>%
  filter(student == "Yes")

prob_student_default <- mean(Default_student$default_binary)
round(prob_student_default, digits = 3)

prob_student_nodefault <- 1 - prob_student_default
round(prob_student_nodefault, digits = 3)

entropy_student <- -1 * (prob_student_default*log(prob_student_default, base = 2) +
                           prob_student_nodefault*log(prob_student_nodefault, base = 2))
round(entropy_student, digits = 3)                         

###not student:
Default_notstudent <- Default %>%
  filter(student == "No")

prob_notstudent_default <- mean(Default_notstudent$default_binary)
round(prob_notstudent_default, digits = 3)

prob_notstudent_nodefault <- 1 - prob_notstudent_default
round(prob_notstudent_nodefault, digits = 3)

entropy_notstudent <- -1 * (prob_notstudent_default*log(prob_notstudent_default, base = 2) +
                              prob_notstudent_nodefault*log(prob_notstudent_nodefault, base = 2))
round(entropy_notstudent, digits = 3)                         

###weighted average:
nrow(Default_student)
nrow(Default_notstudent)

entropy_student_split <- entropy_student*(2944 / (2944 + 7056)) + entropy_notstudent*(7056 / (2944 + 7056))
round(entropy_student_split,digits = 3)

##information gain
info_gain <- entropy_whole_sample - entropy_student_split
round(info_gain, digits = 3)

#Question 16
#Next, create training and testing data sets.  Your code should do the following:
#Set a seed equal to 13215 exactly like we did in class, using “Mersenne-Twister” as the algorithm and “Rejection” as the sample.kind.
#Draw a random sample of rows equal in length to 60% of the number of rows in your data. 
#Create a training data set with the rows in your random sample.  (Hint:  name this data set something like training_data_Default to make sure you don’t accidentally re-use your earlier training data set.)
#Create a testing data set with the rows not in your random sample. (Hint:  name this data set something like test_data_Default to make sure you don’t accidentally re-use your earlier training data set.)
set.seed(13215, "Mersenne-Twister", sample.kind = "Rejection")

rows <- sample(nrow(Default), 0.6*nrow(Default))

training_data_Default <- Default[rows,]
test_data_Default <- Default[-rows,]

dim(training_data_Default)
dim(test_data_Default)

head(rows, 5)

#Question 17
#Estimate a forest of classification trees with your training data set and default as the outcome variable (y-variable) and the following explanatory variables:  income, student, and balance.  Immediately before running your forest, set a seed of 13215. 
set.seed(13215)

forest_Default <- randomForest(default ~ student + balance + income, 
                   data = training_data_Default, proximity = TRUE)

print(forest_Default)
varImpPlot(forest_Default)

#Question 18
#Use your testing data to predict outcome (default).  Create a confusion matrix using your actual outcomes and predicted outcomes.  Fill in the blanks:
test_data_Default <- test_data_Default %>%
  mutate(forest_predictions = predict(forest_Default, test_data_Default, type = "response"))

Confusion_matrix <- table(test_data_Default$default_binary, test_data_Default$forest_predictions)
Confusion_matrix

#accuracy:
sum(diag(Confusion_matrix)) / sum(Confusion_matrix)

precision <- 33 / (33+9)
round(precision, digits = 3)

recall <- 33 / (106 + 33)
round(recall, digits = 3)

F_score <- 2 * (precision*recall)/(precision +recall)
round(F_score, digits = 3)

#Question 19
#Use ggplot to plot a histogram of the number of nodes per tree in your forest.  Select an appropriate binwidth and label your axes appropriately.  Save your figure as a pdf, 6 inches by 6 inches in dimension.  Upload your graph below.
tree_size_rf <- tibble(size = treesize(rf_1))

plot_2 <- ggplot(data = tree_size_rf, aes(x = size)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "number of notes", y = "count")

ggsave(filename = "Desktop/plot_2.pdf",
       plot = plot_2,
       height = 6,
       width = 6,
       units = c("in"))

#Question 20
#For comparison with your forest of classification trees, estimate a logistic model with your training data set and default as the outcome variable (y-variable) and the following explanatory variables:  income, student, and balance. 
logit_1 <- glm(default_binary ~ student + balance + income,
               data = training_data_Default, 
               family = binomial(link = "logit"))

summary(logit_1)

round(logit_1$coefficients, digits = 3)

#Question 21
#Use your testing data and logistic model to predict probabilities of default.  What is the average predicted probability of default?  Round to three decimal places.
test_data_Default <- test_data_Default %>%
  mutate(logit_predictions = predict(logit_1, test_data_Default, type = "response")) %>%
  mutate(logit_predictions_binary = ifelse(logit_predictions > 0.5, 1, 0))

round(mean(test_data_Default$logit_predictions), digits = 3)

#Question 22
#Use the predicted probabilities from your logistic model to predict whether each borrower defaults by creating a variable equal to one if the borrower’s predicted probability of default is greater than 0.5. 
logit_Confusion_Matrix = table(test_data_Default$default_binary, test_data_Default$logit_predictions_binary)
logit_Confusion_Matrix

logit_accuracy <- sum(diag(logit_Confusion_Matrix)) / sum(logit_Confusion_Matrix)
round(logit_accuracy, digits = 3)

logit_precision <- 43 / (43+14)
round(logit_precision, digits = 3)

logit_recall <- 43 / (96 + 43)
round(logit_recall, digits = 3)

logit_F_score <- 2 * (logit_precision*logit_recall)/(logit_precision +logit_recall)
round(logit_F_score, digits = 3)