#install the necessary packages
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("Snowballc")
install.packages("ggplot2")
install.packages("topicmodels")
install.packages("proxy")
install.packages("rJava")
install.packages("RWeka")
install.packages("RWekajars")
install.packages("textir")
install.packages("Matrix")
install.packages("tm.plugin.webmining") 

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

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'h6TiOGaGc0G1aKszFMcoRtNSK'
consumer_secret <- 'cif4ylyRAzsa7RScYdNjnpvCEN1tQ4mLrLrnonBtH287EZ9sVx'
access_token <- '53454531-Qzec39lth8VA8UwwqBlp5Bgl9TcoZTMNpqCwHnZiB'
access_secret <- 'jWIc2KOx9PNEV1VHqTSLiubb9p0UlVWU2VWeU31T58aiM'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

r_stats <- searchTwitter("#ABdeVilliers", n=5000, lang="en")
r_stats1 <- searchTwitter("#MicromaxCanvasSelfie", n=5000, lang="en")
r_stats2 <- searchTwitter("#UnionBudget2015", n=5000, lang="en")
r_stats3 <- searchTwitter("#INDvsUAE", n=5000, lang="en")
r_stats4 <- searchTwitter("#ITrustAK", n=5000, lang="en")
r_stats5 <- searchTwitter("#RIPResearch", n=5000, lang="en")




y_1.df <- do.call("rbind", lapply(r_stats, as.data.frame))
y_2.df <- do.call("rbind", lapply(r_stats1, as.data.frame))
y_3.df <- do.call("rbind", lapply(r_stats2, as.data.frame))
y_4.df <- do.call("rbind", lapply(r_stats3, as.data.frame))
y_5.df <- do.call("rbind", lapply(r_stats4, as.data.frame))
y_6.df <- do.call("rbind", lapply(r_stats5, as.data.frame))

y.df <- rbind(y_1.df,y_2.df,y_3.df,y_4.df,y_5.df,y_6.df)
y.df <- sapply(y.df, function(x) iconv(x, "latin1", "ASCII", sub=""))
dim(y.df)

#create corpus
r_stats_text_corpus <- Corpus(VectorSource(y.df))

#clean up
v0 <- tm_map(r_stats_text_corpus, stripWhitespace)

v0 <- tm_map(v0, content_transformer(tolower))

v0 <- tm_map(v0, removePunctuation)

removeURLs <- function(x) gsub("http[[:alnum:]]*", "", x)
v0 <- tm_map(v0, removeURLs)

myStopWords <- c(stopwords('english'),"RT","...","......")
v0 <- tm_map(v0, removeWords, myStopWords)

#Savinng Data
save("v0", file='d:/R/Tweets_Corpus_v0.Rdata')

#Generating Matrix
v0_1 <- tm_map(v0, PlainTextDocument)
v0_2 <- Corpus(VectorSource(v0))
tweets_TmDoc_v0 <- TermDocumentMatrix(v0_1, control=list(wordLengths=c(1, Inf)))

#Limiting Terms

sp <- findFreqTerms(tweets_TmDoc_v0, highfreq=5)

#Removing words
LessThanFive <- c(sp,"RT","...")
v1 <- tm_map(v0, removeWords, LessThanFive)


v1_1 <- tm_map(v1, PlainTextDocument)

#Generating Matrix
tweets_TmDoc_v1 <- TermDocumentMatrix(v1, control=list(wordLengths=c(1, Inf)))

#Hardik's Code

TFIDF <- tfidf(tweets_TmDoc_v0, normalize = TRUE)
n <- dim(TFIDF)[1]
centroids <- matrix(data = NA,n,6)


centroids[,1] <- rowSums(TFIDF[,1:5000])/5000 
centroids[,2] <- rowSums(TFIDF[,5001:10000])/5000 
centroids[,3] <- rowSums(TFIDF[,10001:15000])/5000 
centroids[,4] <- rowSums(TFIDF[,15001:20000])/5000 
centroids[,5] <- rowSums(TFIDF[,20001:25000])/5000 
centroids[,6] <- rowSums(TFIDF[,25001:30000])/5000 



#K MEans Clustering

# remove sparse terms 
tdm2 <- removeSparseTerms(tweets_TmDoc_v0, sparse = 0.998) 
m2 <- as.matrix(tdm2)
# cluster terms 
distMatrix <- dist(scale(m2)) 
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
#cut tree into 6 clusters 
rect.hclust(fit, k = 6)

m3 <- t(m2) # transpose the matrix to cluster documents (tweets) 
set.seed(122) # set a fixed random seed 
k <- 6 # number of clusters 
kmeansResult <- kmeans(m3, k) 
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k)
{ 
	cat(paste("cluster ", i, ": ", sep = ""))
	s <- sort(kmeansResult$centers[i, ], decreasing = T) 
	cat(names(s)[1:5], "\n") 
	# print the tweets of every cluster 
	# print(tweets[which(kmeansResult£cluster==i)]) 
}



























r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)

r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
removeURLs <- function(x) gsub("http[[:alnum:]]*", "", x)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeURLs)

#To remove Stop Words
myStopWords <- c(stopwords('english'),"RT","...")
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeWords, myStopWords)

#To save it to Data
save("r_stats_text_corpus", file='d:/R/Tweets_Corpus.Rdata')


r_stats_text_corpus_copy <- r_stats_text_corpus


#inserting spaces between the Tweets
tweetcorpus.fun <- function(corpus, lower, upper)
{
	for(i in lower:upper)
	{
		cat(paste("[[",i,"]]", sep=""))
		writeLines(strwrap(corpus[i], width=73))
		cat("\n")
	}
}

tweetcorpus.fun(r_stats_text_corpus, 4, 9)


#Coverting It to Plain Text

r_stats_text_corpus <- tm_map(r_stats_text_corpus, PlainTextDocument)
r_stats_text_corpus_copy <- tm_map(r_stats_text_corpus_copy, PlainTextDocument)
	
#To save it to Data
save("r_stats_text_corpus", file='d:/R/Tweets_Corpus.Rdata')

#Generating Matrix
tweets_TmDoc_v0 <- TermDocumentMatrix(v0, control=list(wordLengths=c(1, Inf)))

#Viewing Matrix
tweets_TmDoc
dimnames(tweets_TmDoc)$Terms
max(apply(tweets_TmDoc,1,sum))
which(apply(tweets_TmDoc,1,sum) > 20)

#Indexing 
index <- which(dimnames(tweets_TmDoc)$Terms == "admit")
inspect(tweets_TmDoc[index + (0:6), 60:70])

findFreqTerms(tweets_TmDoc, lowfreq=150)

#Plotting
termFreq <- rowSums(as.matrix(tweets_TmDoc))
termFreq <- subset(termFreq,termFreq >= 20)
qplot(names(termFreq),termFreq, main="Term Frequencies", geom="bar", ylab="Terms",stat="identity")+coord_flip()


tweets_TmDoc2 <- removeSparseTerms(tweets_TmDoc, sparse = 0.95)
tweets_matrix2 <- as.matrix(tweets_TmDoc2)
dimnames(tweets_TmDoc2)$Terms

#WordCLouds
wordFreq.sort <- sort(rowSums(tweets_matrix2), decreasing=T)

set.seed(500)
grayLevels <- gray((wordFreq.sort + 10) / (max(wordFreq.sort) + 10))
word.cloud <- wordcloud(words=names(wordFreq.sort), freq=wordFreq.sort, min.freq=3, random.order=F, colors=grayLevels)

wordcloud(tweets_matrix2)

#removing Sparse Terms

tweets_TmDoc2 <- removeSparseTerms(tweets_TmDoc, sparse = 0.95)
tweets_matrix2 <- as.matrix(tweets_TmDoc2)
dimnames(tweets_TmDoc2)$Terms

load("d:/R/Tweets_Corpus.Rdata")

