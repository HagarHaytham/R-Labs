#hagar haytham sec: 2 B.N. 28

#Lab 8 - Text Mining

rm(list=ls())
setwd("D:\\CUFE\\4B\\Big Data\\Labs\\R-Labs\\Lab 8 - Text Analytics")
#install.packages("tm")
library(tm)   

#1. Import the data in 'movie_reviews.csv' into a dataframe.
df <- read.csv("movie_reviews.csv",header = TRUE)
#2. Inspect the dataframe to get familiar with the dataset.
colnames(df) 
head(df,10)
dim(df)
#3. Construct a corpus object from the 'text' column in the dataframe.
pcorpus <- Corpus(VectorSource(df$text))
pcorpus
inspect(pcorpus[1:2])
#4. Convert all the words of the corpus from uppercase to lowercase
pcorpus <- tm_map(pcorpus, tolower)
pcorpus
inspect(pcorpus[1:2])
#5. Remove punctuation marks, numbers, and white spaces from the corpus.
pcorpus <- tm_map(pcorpus,removePunctuation)
pcorpus <- tm_map(pcorpus,removeNumbers)
pcorpus <- tm_map(pcorpus, stripWhitespace)
pcorpus
inspect(pcorpus[1:2])
#6. Remove stopping words (in English language) from the corpus.
pcorpus <- tm_map(pcorpus, removeWords, stopwords("english"))
inspect(pcorpus[1:2])
#7. Construct the document term matrix.
pdtm <- DocumentTermMatrix(pcorpus)
inspect(pdtm)
#8. Inspect the document term matrix and determine its sparsity percentage.
pdtm
str(inspect(pdtm))
#Non-/sparse entries: 689184/3006202016
#Sparsity           : 100%

#9. Remove sparse terms below a sparsity threshold 0.999
pdtm <- removeSparseTerms(pdtm, 0.9999)
str(inspect(pdtm))
#10. Find words of frequencies higher than 65.
pfreq <- findFreqTerms(pdtm, 65)
pfreq
#11. Find the associations of words "movie" and "live".
#11. Find the associations of words "titanic" and "marvel". [Use correlation limit= 0.05]
Assoc <-findAssocs(pdtm, c("titanic","marvel"), c(0.05,0.05))
Assoc$titanic
Assoc$marvel
#12. Remove again sparse terms below a sparsity threshold 0.999
pdtm <- removeSparseTerms(pdtm, 0.999)
#13. Construct a normal matrix from the term-document matrix and sort the
#words by their frequency in a descending order.
pdtm2 <- as.matrix(pdtm)
psorted <- sort(colSums(pdtm2),decreasing=TRUE)
#14. Display the most frequent five words in the corpus.
pwords <- names(psorted)
pwords[1:5]
psorted[1:5]
#15. Display the word cloud of the first 100 words.
#install.packages("wordcloud")
library(wordcloud)
wordcloud(pwords[1:100], psorted[1:100])
wordcloud(pwords[1:100], psorted[1:100],colors=brewer.pal(6,"Dark2"))
