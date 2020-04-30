# hagar haytham sec. 2 B.N:28

#Lab 3 Association Rule Mining
#1- First of all, start by cleaning the workspace and setting the working directory
rm(list=ls())
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 3 - Association Rule Mining")
getwd()

#2-Load the libraries arules and arulesViz
#install.packages(c("arules", "arulesViz"))
library(arules)
library(arulesViz)

#3- Load the transactions in the file AssociationRules.csv using the function read.transactions.
transactions <- read.transactions("AssociationRules.csv",header = FALSE)
dim(transactions)
#4- Display the transactions in a readable format using the function inspect.
inspect(transactions[1:100]) 
#5-What are the most frequent two items in the dataset? What are their frequencies?
summary(transactions)
sortedTransactions <-sort(itemFrequency(transactions),decreasing = TRUE)
sortedTransactions[1:3]
#most frequent items are:
# item13 with frequency 4948   --->  0.4948
# item5 with frequency 3699    --->  0.3699
#item30 with frequency 3308    --->  0.3308

#6- Plot the 5 most frequent items of the transactions using the function
itemFrequencyPlot(transactions,topN =5)

#7- Generate the association rules from the transactions using the apriori algorithm. Set the minimum support = 0.01,
#minimum confidence = 0.5, minimum cardinality (number of items in the rule) = 2. Use the function apriori
rules <- apriori(transactions, parameter=list(support=0.01, confidence=0.5,minlen=2))
inspect(rules)
#8-Now, sort the generated rules by support. Search the function sort found in the arules package. Show only the first 6 rules.
rulesSupport <- sort(rules, decreasing = TRUE ,by = "support")
inspect(rulesSupport[1:6])
#9-Sort the generated rules by confidence. Show only the first 6 rules.
rulesConfidence <- sort(rules, decreasing = TRUE ,by = "confidence")
inspect(rulesConfidence[1:6])
#10-Sort the generated rules by lift. Show only the first 6 rules.
rulesLift<- sort(rules, decreasing = TRUE ,by = "lift")
inspect(rulesLift[1:6])
#11-Plot the generated rules with support as x-axis, confidence as y-axis and lift as shading. Use the function plot in arules package.
plot(rules, measure = c("support","confidence"), shading = "lift")
#12-Based on (8-11), Can you tell now what are the most interesting rules that are really useful and 
#provide a real business value and an insight to the concerned corporate?

#The most important rules are the rules with high Lift and relatively high confidendence
#The bussiness value is that most items to get interset with are
#items (15,30,56 and 49) these items are the most useful cause they exist in the highest lift rule and the heighest confidence rule
#rule 1 in most high lift means that people who buy 15,30,56 are probably gonna buy item 49 
# rule 3 means that people who buy 15,30,49 will buy item 56
# so these 4 items are nealy bought together most of the time
#the other rules will give us more insights in the same manner 

