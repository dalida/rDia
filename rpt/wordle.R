

install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")

require(NLP)
require(RColorBrewer)
require(tm)
require(wordcloud)
require(SnowballC)

paper<- Corpus(DirSource("/home/lisa/rDia/rpt/wc"))

inspect(paper)

paper <- tm_map(paper, stripWhitespace)
paper <- tm_map(paper, content_transformer(tolower))
paper <- tm_map(paper, removeWords, stopwords("english"))
#paper <- tm_map(paper, stemDocument)
paper <- tm_map(paper, removeNumbers)
paper <- tm_map(paper, removePunctuation)

wordcloud(paper, scale=c(5,0.5), max.words=100, random.order=FALSE,
					rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))