

install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")

library(tm)
library(wordcloud)
library(SnowballC)

paper<- Corpus(DirSource("wc"))

inspect(paper)

paper <- tm_map(paper, stripWhitespace)
paper <- tm_map(paper, content_transformer(tolower))
paper <- tm_map(paper, removeWords, stopwords("english"))
#paper <- tm_map(paper, stemDocument)
paper <- tm_map(paper, removeNumbers)
paper <- tm_map(paper, removePunctuation)

wordcloud(paper, scale=c(5,0.5), max.words=100, random.order=FALSE,
					rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))