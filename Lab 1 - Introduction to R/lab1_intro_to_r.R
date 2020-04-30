#hagar haytham sec.2 B.N. 28

#1- First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())
getwd()
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\Lab 1 - Introduction to R\\Requirement")
getwd()
#2- Import the dataset titanic.csv into a data frame
df <- read.csv("titanic.csv",header = TRUE)
df
#3-a- Show the dimensions of the data frame.
dim(df)
dim.data.frame(df)
#3-b- Show the structure of the data frame.
str(df)
#3-c- Get more insight into data by exploring the first and the last ten rows in the dataset.
head(df,10)
tail(df,10)
#3-d- Show summary of all variables in the data frame.
summary(df)

#4-a- Show a summary for the variable age only.
summary(df$Age)
#4-b- What are the first and third quartile values for this variable? What do these values mean?
#Ans -> 1st quartile is 20.12 it means that the maean of the first half of data(after being sorted) is 20.12 
# 3rd quartile is 38 and it means the mean for the second half of data(after being sorted) is 38
#4-c- Are there any missing values in the variable age? (i.e. written as <NA>)
help("is.na")
is.na(df$Age)
anyNA(df$Age)
#4-d- What is the type of the variable embarked? Show the levels of this variable. Is that what you were expecting?
class(df$Embarked)
typeof(df$Embarked) #??????? INTEGER ????
str(df$Embarked) # 4 levels - expected 3
Embarked_values <- factor(df$Embarked)
Embarked_values
#4-e- Can you conclude what's needed at this step in the data analysis cycle? 
# NEEDS PREPROCESSING

#5-a- Remove the rows containing <NA> in the age variable from the data frame.
df <-subset(df, !is.na(df$Age))
dim(df)
#5-b- Remove the rows containing any unexpected value in the embarked variable from the dataset.
df <-subset(df, df$Embarked =="C" | df$Embarked =="S" | df$Embarked =="Q")
#5-c- Now, check that no NA values exist in the age variable. Also, factor the embarked variable and display its levels. Is that what you are expecting?
is.na(df$Age)
anyNA(df$Age)
str(df$Embarked)
Embarked_values <- factor(df$Embarked)
Embarked_values
#5-d- Some variables are not very interesting and provide no real indicative value. Remove columns Cabin and Ticket from the dataset.
df <- subset(df, select = -c(Cabin, Ticket))
df
dim(df)
#6-a- Show the number of males and females aboard the Titanic.
#males <- subset(df, df$Gender =='male')
#females<- subset(df,df$Gender=='female')
#m<-dim(males)[1]
#f<-dim(females)[1]
#vals <- c(m,f)
males <- sum(df$Gender =='male')
females <- sum(df$Gender=='female')
vals <- c(males,females)
barplot(vals,names.arg=c("Male","Female"))
#6-b- Plot a pie chart showing the number of males and females aboard the Titanic.
pie(vals,labels=c("males","females"))
#6-c- Indicate males with a blue color and females with a red color in the above plot.
barplot(vals,col =c("blue","red"),names.arg=c("Male","Female"))
#6-c2- Show the number of people who survived and didn't survive from each gender.
males_survived <- sum( df$Gender=='male' & df$Survived ==1)
males_didnt <-sum(df$Gender=='male' & df$Survived ==0)
females_survived <-sum( df$Gender=='female'& df$Survived ==1)
females_didnt <-sum(df$Gender=='female'& df$Survived ==0)

values <- c(males_survived,males_didnt,females_survived,females_didnt)
l <- c("males_survived","males_didn't","females_survived","females_didn't")
barplot(values,names.arg = l)
#6-d- Plot a pie chart showing the number of males and females who survived only.
pie(c(males_survived,females_survived),labels = c("males survived","females survived"))
#6-e- What do you conclude from that?
# Females survived more then males so that means that the gender is an important feature to keep to descriminate between the passengers that suvived and the passangers that didn't suvive

#6-f- Show the relationship between social class and survival i.e. show how many people survived and how many people didn't survive from each class.
class1_survived <- sum(df$Survived == 1 & df$Pclass == 1)
class2_survived <- sum(df$Survived == 1 & df$Pclass == 2)
class3_survived <- sum(df$Survived == 1 & df$Pclass == 3)

class1_didnt <- sum(df$Survived == 0 & df$Pclass == 1)
class2_didnt <- sum(df$Survived == 0 & df$Pclass == 2)
class3_didnt <- sum(df$Survived == 0 & df$Pclass == 3)

classvalues <- c(class1_survived,class2_survived,class3_survived,class1_didnt,class2_didnt,class3_didnt)
classl <-c("class1_survived","class2_survived","class3_survived","class1_didn't","class2_didn't","class3_didn't")
#6-g- Plot this relationship as a stacked bar plot.
barplot(classvalues,names.arg = classl)

#6-h- Indicate survived passengers with a blue color and un-survived passengers with a red color in the above plot.
colrs <- c ("blue","blue","blue","red","red","red")
barplot(classvalues,names.arg = classl,col =colrs)
#6-i- What do you conclude from that?
#we concule that also the class is a descriminating factor and it has an impact on the suvived passengers (class 3 has the greatest number of passengers that didn't survive)
#6-j- Plot a box and whiskers plot for the variable age
boxplot(df$Age)
#6-k- What does this plot mean?
#this plot shows the summary for the age feature , it shows the first quartile , median, third quartile
#it also shows the outliers which are above 60 
#6-l- Plot a density distribution for the variable age.
#hist(df$Age)
plot(density(df$Age))

#7- Remove all columns but passenger name and whether they survived or not. Export the new dataset to a file named "titanic_preprocessed.csv"
df <- subset(df, select = c(Name, Survived))
df
write.csv(df,"titanic_preprocessed.csv")
