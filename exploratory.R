library(tm)
library(tidytext)
library(ggplot2)
library(stringi)
library(RWeka)
library(tidyverse)

setwd('/Users/taepark/Dropbox/R/capstone project/data/')

# File name
blog = 'en_US.blogs.txt'
news = 'en_US.news.txt'
twitter = 'en_US.twitter.txt'



# Sampling
getSample = function(filename) {
  base = readLines(filename)
  r = sample(base, length(base)*0.01, replace=F)
  write.table(r, file=file.path('samples', paste('sample_', filename, sep="")), row.names = F)
  return(stri_stats_general(base))
}

blog_info = getSample(blog)
news_info = getSample(news)
twitter_info = getSample(twitter)

# Basic summaries
blog_info
news_info
twitter_info



# Load
cleanFile = function(path, txt) {
  dt = VCorpus(DirSource(path, encoding = 'UTF-8'), readerControl = list(reader = readPlain))
  
  # Basic cleaning
  toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
  dt = tm_map(dt, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
  dt = tm_map(dt, toSpace, "@[^\\s]+")
  dt = tm_map(dt, toSpace, "www[a-z]+com")
  dt = tm_map(dt, toSpace, "“")
  dt = tm_map(dt, toSpace, "”")
  dt = tm_map(dt, toSpace, "‘")
  dt = tm_map(dt, toSpace, "’")
  dt = tm_map(dt, removePunctuation)
  dt = tm_map(dt, removeNumbers)
  
  dt = tm_map(dt, tolower)     
  dt = tm_map(dt, removeWords, stopwords("english"))  
  dt = tm_map(dt, stripWhitespace)   
  dt = tm_map(dt, PlainTextDocument)
  
  return(dt)
}

path = "/Users/taepark/Dropbox/R/capstone project/data/samples"
corpus = cleanFile(path)

# Plot function
getPlot = function(dtm, n=6) {
  mat = as.matrix(dtm)
  if (nrow(mat) > ncol(mat)) {
    freq = rowSums(mat)
  } else {
    freq = colSums(mat)
  }
  freq = sort(freq, decreasing=TRUE)
  df = head(data.frame(word=names(freq), freq), n)
  g = ggplot(df, aes(x=reorder(word, -freq), y=freq)) +
    geom_col() +
    xlab("Word") + 
    ylab("Frequency") +
    ggtitle("The frequency of words appearing") +
    theme(axis.text.x = element_text(angle = 45))
  
  return(g)
}


# Plot
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.99)
getPlot(dtm)

# bigram plot
BigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_dtm = TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
bigram_dtm = removeSparseTerms(bigram_dtm, 0.99)
getPlot(bigram_dtm, 10)

# prediction
df = tidy(dtm) %>% arrange(desc(count))
df1 = tidy(bigram_dtm) %>% arrange(desc(count))



df %>% filter(grepl('case' ,term))
df1 %>% filter(grepl('case ' ,term))

tokens = lapply(corpus, scan_tokenizer)
stem = lapply(tokens, stemCompletion, corpus)


bi = tidy(bigram_dtm)

colnames(df) = 'word'
df1 =tidy(dtm) %>% count(count, sort=T) 
head(df)

dtm$dimnames$Docs = as.character(1:3)
dtm$dimnames$Terms
bigram_dtm$dimnames$Docs = as.character(1:3)
bigram_dtm$dimnames$Terms
inspect(dtm)
inspect(bigram_dtm)
findAssocs(dtm, 'case', corlimit=0.9)


df2 = tidy(dtm) %>% select(term) %>% unnest_tokens(term)

df1 = data.frame(docs = dtm$dimnames$Docs, as.matrix(dtm), row.names=NULL, check.names=FALSE)

head(df1)


