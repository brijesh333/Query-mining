library("twitteR")
library("wordcloud")
library("tm")
library("RMySQL")
library("SnowballC")
library("ggplot2")
library("topicmodels")
library("proxy")
library("rJava")
library("RWeka")
library("RWekajars")
library("textir")
library("Matrix")
library("tm.plugin.webmining") 
library("SnowballC")

Ndocs<- read.csv("/media/brijesh/study/DAIICT/Study/SEM 2/IR Project/Dataset/Testing dataset/200fake.csv")
Ndocs <- Ndocs[-grep("AnonID",colnames(Ndocs))]
Ndocs <- Ndocs[-grep("QueryTime",colnames(Ndocs))]
Ndocs <- Ndocs[-grep("ItemRank",colnames(Ndocs))]
Ndocs <- Ndocs[-grep("ClickURL",colnames(Ndocs))]
doc.list<-Ndocs

doc.list<-t(doc.list)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))

query <- "india"

library(tm)

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus1 <- Corpus(my.docs)
my.corpus1

getTransformations()

my.corpus1 <- tm_map(my.corpus1, removePunctuation)
my.corpus1$doc1

library(SnowballC)
my.corpus1 <- tm_map(my.corpus1, stemDocument)
my.corpus1$doc1 

my.corpus1 <- tm_map(my.corpus1, removeNumbers)
my.corpus1 <- tm_map(my.corpus1, removeWords, stopwords("english")) 
my.corpus1 <- tm_map(my.corpus1, PlainTextDocument)
my.corpus1 <- tm_map(my.corpus1, stripWhitespace)
my.corpus1 <- tm_map(my.corpus1, content_transformer(tolower))


my.corpus1$doc1

tdm <- TermDocumentMatrix(my.corpus1)
inspect(tdm[0:4,])


term.doc.matrix <- as.matrix(tdm)

get.tf.idf.weights <- function(tf.vec, df) {
  
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.docs] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]

angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:3, ]

query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

##Final result
View(results.df)