
library(tm)
library(wordcloud)
library(syuzhet)

reviews <- read.csv(file.choose(), header = T)

str(reviews)

corpus <- iconv(reviews$text)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("book", "read", "life"))

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, stemDocument)
inspect(corpus[1:5])

reviews_final <- corpus

tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]

w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, las = 2, col = "blue")

w <- sort(rowSums(tdm), decreasing = T)
set.seed(1234)
wordcloud(words = names(w) ,freq = w, max.words = 100, random.order = F, min.freq = 5,
          colors = brewer.pal(8,"Dark2"),scale = c(3,0,.3))

sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

s$score <- s$positive - s$negative
s[1:10,]

write.csv(x = s, file = "C:/Users/Abhirath/Desktop/Final_score.csv")

review_score <- colSums(s[,])
print(review_score)

barplot(colSums(s[,1:10]), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment')

tdm1 <- TermDocumentMatrix(reviews_final)

findAssocs(tdm1, terms = c("good","great","happi"), corlimit=0.19)

findAssocs(tdm1, terms = findFreqTerms(tdm1, lowfreq = 50), corlimit = 0.25)
