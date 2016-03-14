library(XML) 
library(SnowballC)
library(tm)
xml.url <- "http://www.isical.ac.in/~fire/data/topics/adhoc/en.topics.26-75.2008.txt" 
xmlfile <- xmlTreeParse(xml.url)
class(xmlfile)
xmltop = xmlRoot(xmlfile) 
print(xmltop)[1:2]

plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue)) 

plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:2]

plantcat_df$num <- NULL
plantcat_df$desc <- NULL
plantcat_df$narr <- NULL
write.table(plantcat_df,file="/media/brijesh/study/DAIICT/Study/SEM 2/IR Project/Dataset/Testing dataset/fire_data_4.csv")

doc.list<-plantcat_df
doc.list<-t(doc.list)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))

##a <- function(){readline("Enter query : ")}
##query <- as.character(a())

query <- "india"



my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus <- Corpus(my.docs)
my.corpus

getTransformations()

my.corpus <- tm_map(my.corpus, removePunctuation)


my.corpus <- tm_map(my.corpus, stemDocument)

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, PlainTextDocument)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus$doc1

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:14,0:10 ])
tdm<-t(term.doc.matrix.stm)
term.doc.matrix <- as.matrix(term.doc.matrix.stm)

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

##       
## Terms   doc1  doc2 doc3 doc4  doc5 doc6 doc7 query
##   all  2.807 0.000    0    0 0.000    0    0     0
##   and  0.000 0.000    0    0 2.807    0    0     0
##   anim 0.000 2.807    0    0 0.000    0    0     0

angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:3, ]

##       
## Terms    doc1   doc2 doc3 doc4   doc5 doc6 doc7 query
##   all  0.3632 0.0000    0    0 0.0000    0    0     0
##   and  0.0000 0.0000    0    0 0.3486    0    0     0
##   anim 0.0000 0.3923    0    0 0.0000    0    0     0

query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

##Final result
View(results.df)


cluster_form<-kmeans(tdm,3)
library("cluster")
library("fpc")
plot(prcomp(tdm)$x, col=cluster_form$cluster)
hc <- hclust(dist(tdm),"ave")
plot(hc,hang=-1)
