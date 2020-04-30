# hagar haytham sec. 2 B.N:28

#Lab 4 - Decision Trees
rm(list=ls())
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 4 - Classification\\Lab")
getwd()

#install.packages(c("ROCR", "rpart.plot"))
library("rpart")
library("rpart.plot")
library("ROCR")

#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 1)

summary(fit)
#######################################################################################
# Q1: what is the defult value for split?                                      
#->The split default value is gini  

# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
#the minimum number of observations that must exist in a node in order for a split to be attempted = 2
#          2- "maxdepth=3" 
#the maximum depth of any node of the final tree, with the root node counted as depth 0 = 3
#          3- "minbucket=4" 
#the minimum number of observations in any terminal <leaf> node = 4

# Support your answers with graphs for different values of these parameters.
#---------------------------------------------------------------------------------#
##MINSPLIT Test --> the min number of examples to exist for a node to split it
fitminsplit4 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=4, maxdepth = 3),
             parms=list(split='information'))
rpart.plot(fitminsplit3, type = 4, extra = 1)


fitminsplit7 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=7, maxdepth = 3),
             parms=list(split='information'))
rpart.plot(fitminsplit4, type = 4, extra = 1)
#----------------------------------------------------------------------------------#
#MAXDEPTH TEST---> the maximum depth of any node of the final tree, with the root node counted as depth 0
fitmaxDepth2 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 2),
             parms=list(split='information'))
rpart.plot(fitmaxDepth2, type = 4, extra = 1)
#tree of depth 2

fitmaxDepth1 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
                      method="class", 
                      data=play_decision,
                      control=rpart.control(minsplit=2, maxdepth = 1),
                      parms=list(split='information'))
rpart.plot(fitmaxDepth1, type = 4, extra = 1)
#tree of depth 1
#---------------------------------------------------------------------------------#
#MINBUCKET = 
#the minimum number of observations in any terminal <leaf> node 
# if minbucket is not specified it is set to to minsplit/3
fitminbucket4 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3,minbucket=4),
             parms=list(split='information'))
rpart.plot(fitminbucket4, type = 4, extra = 1)
#here the last nodes has at least 4 nodes 


fitminbucket3 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
                       method="class", 
                       data=play_decision,
                       control=rpart.control(minsplit=2, maxdepth = 3,minbucket=3),
                       parms=list(split='information'))
rpart.plot(fitminbucket3, type = 1, extra = 1)
# here the last nodes has at least 3 observations

#--------------------------------------------------------------------------------#


#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?
#if minsplit is only specified minbucket will be minsplit/3
#if minbucket is only specified minsplit will be minbucket*3

#Q4: What does 'type' and 'extra' parameters mean in the plot function?
#-------------------------Type----------------------------------------#
if (FALSE){
  "Type of plot. Possible values:
    
    0 Draw a split label at each split and a node label at each leaf.
  
  1 Label all nodes, not just leaves. Similar to text.rpart's all=TRUE.

2 Default. Like 1 but draw the split labels below the node labels. Similar to the plots in the CART book.

3 Draw separate split labels for the left and right directions.

4 Like 3 but label all nodes, not just leaves. Similar to text.rpart's fancy=TRUE. See also clip.right.labs.
  5 New in version 2.2.0. Show the split variable name in the interior nodes."
}
#type of the graph , its shape
rpart.plot(fit, type = 0, extra = 1)
rpart.plot(fit, type = 1, extra = 1)
rpart.plot(fit, type = 2, extra = 1)
rpart.plot(fit, type = 3, extra = 1)
rpart.plot(fit, type = 4, extra = 1)


#--------------------------EXTRA----------------------------------------------#
if (FALSE){
"Display extra information at the nodes. Possible values:
0 No extra information.
1 Display the number of observations that fall in the node (per class for class objects; prefixed by the number of events for poisson and exp models). Similar to text.rpart's use.n=TRUE.
2 Class models: display the classification rate at the node, expressed as the number of correct classifications and the number of observations in the node.
Poisson and exp models: display the number of events.
3 Class models: misclassification rate at the node, expressed as the number of incorrect classifications and the number of observations in the node.
4 Class models: probability per class of observations in the node (conditioned on the node, sum across a node is 1).
5 Class models: like 4 but don't display the fitted class.
6 Class models: the probability of the second class only. Useful for binary responses.
7 Class models: like 6 but don't display the fitted class.
8 Class models: the probability of the fitted class.
9 Class models: The probability relative to all observations - the sum of these probabilities across all leaves is 1. This is in contrast to the options above, which give the probability relative to observations falling in the node - the sum of the probabilities across the node is 1.
10 Class models: Like 9 but display the probability of the second class only. Useful for binary responses.
11 Class models: Like 10 but don't display the fitted class.
+100 Add 100 to any of the above to also display the percentage of observations in the node. For example extra=101 displays the number and percentage of observations in the node. Actually, it's a weighted percentage using the weights passed to rpart."
}
rpart.plot(fit, type = 4, extra = 0)
rpart.plot(fit, type = 4, extra = 1)
rpart.plot(fit, type = 4, extra = 2)
rpart.plot(fit, type = 4, extra = 3)
rpart.plot(fit, type = 4, extra = 4)
rpart.plot(fit, type = 4, extra = 5)
rpart.plot(fit, type = 4, extra = 6)
rpart.plot(fit, type = 4, extra = 7)
rpart.plot(fit, type = 4, extra = 8)
rpart.plot(fit, type = 4, extra = 9)
rpart.plot(fit, type = 4, extra = 10)
rpart.plot(fit, type = 4, extra = 11)
rpart.plot(fit, type = 4, extra = +100)


#Q5: Plot the tree with propabilities instead of number of observations in each node.
rpart.plot(fit, type = 4, extra = 9)

######################################################################################
 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case?
# Play (yes)
#Q7: State the sequence of tree node checks to reach this class (label).
#the root checks for tempreture
#Tempreture = mild So goes to left node (left subtree)
#Outlook = overcast so goes to right node which predicts he can play 
# Temprture=mild --> outlook = overcast -> play 
## ================================= END ===================================== ##