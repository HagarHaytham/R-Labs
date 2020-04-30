# hagar haytham sec. 2 B.N:28

#Lab 4 - Classification
#---------------------------Requirement 1---------------------------------------#
#1- First of all, start by cleaning the workspace and setting the working directory
rm(list=ls())
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 4 - Classification\\Lab")
getwd()

#2- Import the dataset nbtrain.csv into a data frame. What are the variables of this dataset?
df <- read.csv("nbtrain.csv",header = TRUE)
colnames(df) # the variables are "age"    "gender" "educ"   "income"

#3- Divide the data into two data frames: a training set containing the first 9000 rows,and a test set containing the remaining rows.
#Why do we split data into training and test sets?
dim(df)
train <- df[1:9000,]
test <- df[9001:dim(df)[1],]
dim(train)
dim(test)
# we split data into train and test so we can have data that the model isn't exsposed to it to evalute its accuracy 
#to untrained data to see if it is overfitting or underfitting or just fitting in the right manner
# Measure the actual performance and then tune the hyperparameters

#4-Train a Naïve Bayes Classifier model with income as the target variable and all other variables as independent variables. 
#Smooth the model with Laplace smoothing coefficient = 0.01
#What does Laplace smoothing coefficient mean?
#install.packages("e1071")
library("e1071")
model <-naiveBayes(income~.,train,laplace=.01)
#Laplace smoothing is used to avois the problem of zero probabilities.
#If we want to calculate P(income = 10-50K | gender = F)
#but assume this example doesn't appear in the training set at all which causes the prob to be zero,
#this will wipe the information of other probabilities. 
#So Laplace is used to smooth categorical data,we always add this smoothing fator to the probability so that no probability would be ever zero.

#5-Display the resulting model.
model

#6-Use the model to predict the income values of the test data
results <- predict (model, test)
results

#7-Display a confusion matrix for the predict values of the test data versus the actual values. Investigate the results.
#Explain the variation in the model's classification power across income classes.
#install.packages("caret")
#library(caret)
#cfm <-confusionMatrix(results,test$income)
#cfm$table
cfm <- table(results,test$income)
cfm

#The model is almost predicting everything to be in class 10-50K
#It got 797 right but 127 were supposed to be 50-80 and 67 were supposed to be GT 80
#So it is biased towards 10-50K class against the other classes 
#Actually it didn't predict any example to be 50-80K class althogh there were supposed to be 127 example in that class
#It predicted only 8 to be in class GT 80K and missclassified the other 67 to be in the class which it is biased to
#8-Display the accuracy of the model. Comment on the result.
Tp <- sum(diag(cfm))
total<-sum(cfm)
accuracy <- Tp/total
accuracy
#the acuracy is reasonable given that many examples to be classified were in the class to which the model is biased
# so we got a sort of high accuracy but The model is biased towards the 10-50K

#9-Display the overall 10-50K, 50-80K, GT 80K misclassification rates.
misclassificationRate <- 1 - accuracy
misclassificationRate
