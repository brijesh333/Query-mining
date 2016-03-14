doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

doc.list <- list(doc1, doc2, doc3, doc4, doc5, doc6, doc7)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))

query <- "Healthy cat food"

library(tm)

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus <- Corpus(my.docs)
my.corpus

## A corpus with 8 text documents

getTransformations()
## [1] "as.PlainTextDocument" "removeNumbers"        "removePunctuation"   
## [4] "removeWords"          "stemDocument"         "stripWhitespace"


my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus$doc1

## Stray cats are running all over the place I see 10 a day

install.packages("SnowballC")
library(SnowballC)
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus$doc1 

## Stray cat are run all over the place I see 10 a day

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, PlainTextDocument)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$doc1

## stray cat are run all over the place i see a day

## The vector space model

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:14, ])

## A term-document matrix (14 terms, 8 documents)
## 
## Non-/sparse entries: 21/91
## Sparsity           : 81%
## Maximal term length: 8 
## Weighting          : term frequency (tf)
## 
##           Docs
## Terms      doc1 doc2 doc3 doc4 doc5 doc6 doc7 query
##   all         1    0    0    0    0    0    0     0
##   and         0    0    0    0    1    0    0     0
##   anim        0    1    0    0    0    0    0     0
##   are         1    1    0    0    0    0    0     0
##   arnold      0    0    0    0    0    1    0     0
##   around      0    0    0    1    0    0    0     0
##   best        0    0    1    1    0    0    0     0
##   billion     0    1    0    0    0    0    0     0
##   brand       0    0    0    1    2    0    0     0
##   buy         0    0    0    0    1    0    0     0
##   came        0    0    0    0    0    1    0     0
##   cat         1    1    0    2    3    0    0     1
##   classic     0    0    0    0    0    1    0     0
##   columbus    0    0    1    0    0    0    0     0


term.doc.matrix <- as.matrix(term.doc.matrix.stm)

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.")

## Dense matrix representation costs 6688 bytes.
##  Simple triplet matrix representation costs 5808 bytes.

get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}

cat("A word appearing in 4 of 6 documents, occuring 1, 2, 3, and 6 times, respectively: \n", 
    get.tf.idf.weights(c(1, 2, 3, 0, 0, 6), 4))

## A word appearing in 4 of 6 documents, occuring 1, 2, 3, and 6 times, respectively: 
##  0.8074 1.615 2.087 0 0 2.894

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
