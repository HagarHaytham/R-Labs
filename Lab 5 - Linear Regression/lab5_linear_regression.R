#Lab 5 Linear Regression

setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 5 _ 6 - Regression\\Lab 5 - Linear Regression")
rm(list=ls())

#=============================Part(1)=====================================
x <- runif(100, 0, 10)     # 100 draws between 0 & 10

#(Q1) Try changing the value of standard deviation (sd) in the next command 
#How do the data points change for different values of standard deviation?
y <- 5 + 6*x + rnorm(100, sd = 2)  # default values for rnorm (mean = 0 and sigma = 1)

#Plot it
plot (x,y)

y1 <- 5 + 6*x + rnorm(100, sd = 1) 
y2 <- 5 + 6*x + rnorm(100, sd = 3) 
y3 <- 5 + 6*x + rnorm(100, sd = 7) 
y4 <- 5 + 6*x + rnorm(100, sd = 0) 
plot (x,y3)
# OLS model
# OLS : Ordinary Least Squares
model1 <- lm(y ~ x)
# Learn about this object by saying ?lm and str(d)

# Compact model results
print(model1)
#(Q2) How are the coefficients of the linear model affected by changing the value
#of standard deviation in Q1?
model2 <- lm(y3 ~ x)
print(model2)

# Regression diagnostics --
ypred <- predict(model1) # use the trained model to predict the same training data
# Learn about predict by saying ?predict.lm

par(mfrow=c(1,1))
plot(y,y, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
points(y, ypred) # plotting the predicted points
# the nearer to the ideal line the better

# Detailed model results
d1 <- summary(model1)
print(d1)

#(Q3) How is the value of R-squared affected by changing the value
#of standard deviation in Q1?

# Learn about this object by saying ?summary.lm and by saying str(d)
cat("OLS gave slope of ", d1$coefficients[2,1],   
    "and an R-sqr of ", d1$r.squared, "\n")

#Graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model1, 1) # plot one diagnostic graphs
#--------------------------------------#
#sd=7
ypred2 <- predict(model2) 

par(mfrow=c(1,1))
plot(y3,y3, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
points(y3, ypred2)
d2 <- summary(model2)
print(d2)
cat("OLS gave slope of ", d2$coefficients[2,1],   
    "and an R-sqr of ", d2$r.squared, "\n")
par(mfrow=c(1,1)) 
plot(model2, 1)

#(Q4)What do you conclude about the residual plot? Is it a good residual plot?
#========================End of Part(1)==============================================

#========================Part(2)=====================================================
#Training a linear regression model
x1 <- runif(100) 
# introduce a slight nonlinearity
#(A)
y1 = 5 + 6*x1 + 0.8*x1*x1 + rnorm(100)
plot(x1,y1)
model <- lm(y1 ~ x1)

summary(model)

#Creating a test set (test vector)

#EDIT: We renamed the variable as x1 instead of xtest (in previous versions)
#becaues the lm function searches in the formula for variables named 
#with x1 and not any other name.
#So, if you used xtest, the lm function will not know what is xtest and
#a random plot will be generated. 

x1 <- runif(100)
#(B)
ytrue = 5 + 6*x1 + 0.8*x1*x1 + rnorm(100)  # same equation of y1 but on xtest to get true y for xtest

ypred <- predict(model, data.frame(x1))

par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)

# graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model, 1) # plot the diagnostic graphs

#(Q5)What do you conclude about the residual plot? Is it a good residual plot?

#(Q6)Now, change the coefficient of the non-linear term in the original model for (A) training 
#and (B) testing to a large value instead. What do you notice about the residual plot?
#===============================End of Part(2)=============================================

#=================================Part(3)==================================================
#(Q7) Import the dataset LungCapData.tsv. What are the variables in this dataset?
df <- read.csv("LungCapData.tsv", sep = "\t")
colnames(df)
# The variable names are "LungCap"   "Age"       "Height"    "Smoke"     "Gender"    "Caesarean"
#(Q8) Draw a scatter plot of Age (x-axis) vs. LungCap (y-axis). Label x-axis "Age" and y-axis "LungCap"
par(mfrow=c(1,1)) # parameters for the next plot
plot(df$Age, df$LungCap, type="p", xlab="Age", ylab="LungCap")

#(Q9) Draw a pair-wise scatter plot between Lung Capacity, Age and Height. 
#Check the slides for how to plot a pair-wise scatterplot
pairs(df$LungCap~df$Age+df$Height)

#(Q10) Calculate correlation between Age and LungCap, and between Height and LungCap.
#Hint: You can use the function cor
cor(df$Age, df$LungCap)
cor(df$Height, df$LungCap)
#(Q11) Which of the two input variables (Age, Height) are more correlated to the 
#dependent variable (LungCap)?


#(Q12) Do you think the two variables (Height and LungCap) are correlated ? why ?

#(Q13) Fit a liner regression model where the dependent variable is LungCap 
#and use all other variables as the independent variables

fit <- lm(df$LungCap~df$Age+df$Height+df$Smoke+df$Gender+df$Caesarean)
#(Q14) Show a summary of this model
d <- summary(fit)
#(Q15) What is the R-squared value here ? What does R-squared indicate?
d$r.squared
#(Q16) Show the coefficients of the linear model. Do they make sense?
#If not, which variables don't make sense? What should you do?
d$coefficients[,1]
#(Q17) Redraw a scatter plot between Age and LungCap. Display/Overlay the linear model (a line) over it.
#Hint: Use the function abline(model, col="red").
#Note (1) : A warning will be displayed that this function will display only the first two 
#           coefficients in the model. It's OK.
#Note (2) : If you are working correctly, the line will not be displayed on the plot. Why?
par(mfrow=c(1,1))
plot(df$Age,df$LungCap, type="p", xlab="Age", ylab="LungCap") 
abline(fit,col="red")


#(Q18)Repeat Q13 but with these variables Age, Smoke and Cesarean as the only independent variables.
fit2 <- lm(df$LungCap~df$Age+df$Smoke+df$Caesarean)
#(Q19)Repeat Q16, Q17 for the new model. What happened?
d2 <- summary(fit2)
d2$coefficients[,1]

par(mfrow=c(1,1))
plot(df$Age,df$LungCap, type="p", xlab="Age", ylab="LungCap") 
abline(fit2,col="red")
#(Q20)Predict results for this regression line on the training data.
predicted <- predict(fit2)
predicted
#(Q21)Calculate the mean squared error (MSE)of the training data.
ms <- mean(fit$residuals^2)
ms
#-----#
install.packages("Metrics")
library("Metrics")
err <- mse(df$LungCap,predicted)
err


