library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(ggplot2)
library(ROAuth)
library(textir)


api <- "Rxy7haYRPXPVVjJl80yEzftVz"
api_secret <- "ontlD8mCDaoaWwSCKDbIIkMm5WDqQj8960Ve2wOuVf9XVHWrK7"
token <- "823290030-t7zVklA2yfiTHjb0TP5zGR9iHxm4Q2ble3fYFYBX"
token_secret <- "qFIJjnljHoqcB015lERX5hkJxPhwy191FtIYF5zdwEx3m"

setup_twitter_oauth(api, api_secret, token, token_secret)

tweets1 <- searchTwitter("#japan", n=5000, lang="en")
df00 <- do.call("rbind", lapply(tweets1, as.data.frame))$text

tweets1 <- searchTwitter("#obama", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#libya", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#yahoo", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("ipad", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#google", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#facebook", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#Oscars", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#oil", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#gold", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

tweets1 <- searchTwitter("#BringBackOurGirl", n=5000, lang="en")
df01 <- do.call("rbind", lapply(tweets1, as.data.frame))$text
df00 <- rbind(df00,df01)

df1 <- do.call("rbind", lapply(tweets1, as.data.frame))
df2 <- do.call("rbind", lapply(tweets2, as.data.frame))
df3 <- do.call("rbind", lapply(tweets3, as.data.frame))
df4 <- do.call("rbind", lapply(tweets4, as.data.frame))
df5 <- do.call("rbind", lapply(tweets5, as.data.frame))
df6 <- do.call("rbind", lapply(tweets6, as.data.frame))
df7 <- do.call("rbind", lapply(tweets7, as.data.frame))
df8 <- do.call("rbind", lapply(tweets8, as.data.frame))
df9 <- do.call("rbind", lapply(tweets9, as.data.frame))
df10 <- do.call("rbind", lapply(tweets10, as.data.frame))
df11 <- do.call("rbind", lapply(tweets11, as.data.frame))
df12 <- do.call("rbind", lapply(tweets12, as.data.frame))
df13 <- do.call("rbind", lapply(tweets13, as.data.frame))
df14 <- do.call("rbind", lapply(tweets14, as.data.frame))
df15 <- do.call("rbind", lapply(tweets15, as.data.frame))
df16 <- do.call("rbind", lapply(tweets16, as.data.frame))
df17 <- do.call("rbind", lapply(tweets17, as.data.frame))
df18 <- do.call("rbind", lapply(tweets18, as.data.frame))
df19 <- do.call("rbind", lapply(tweets19, as.data.frame))
df20 <- do.call("rbind", lapply(tweets20, as.data.frame))
df21 <- do.call("rbind", lapply(tweets21, as.data.frame))
df22 <- do.call("rbind", lapply(tweets22, as.data.frame))
df23 <- do.call("rbind", lapply(tweets23, as.data.frame))
df24 <- do.call("rbind", lapply(tweets24, as.data.frame))
df25 <- do.call("rbind", lapply(tweets25, as.data.frame))
df26 <- do.call("rbind", lapply(tweets26, as.data.frame))
df27 <- do.call("rbind", lapply(tweets27, as.data.frame))
df28 <- do.call("rbind", lapply(tweets28, as.data.frame))
df29 <- do.call("rbind", lapply(tweets29, as.data.frame))
df30 <- do.call("rbind", lapply(tweets30, as.data.frame))

df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20, df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)

tweets.corpus <- Corpus(VectorSource(df$text))
tweets.corpus
getTransformations()

tweets.corpus <- tm_map(tweets.corpus, tolower)

