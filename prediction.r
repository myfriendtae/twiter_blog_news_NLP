library(tidyverse)
library(tidytext)
library(tm)
setwd('/Users/taepark/Dropbox/R/capstone project/data/')
data(stop_words)

# File name
blog = 'en_US.blogs.txt'
news = 'en_US.news.txt'
twitter = 'en_US.twitter.txt'
nword=2
test_word = tail(test_df, 3)[3,1]
findFreqWord = function(base_df, test_word, options, nword) {
  comb = tibble()
  test_df = tibble(text = test_word) %>% unnest_tokens(word, text, token = "ngrams", n = nword) %>% anti_join(stop_words, by = "word")
  for (test_word in tail(test_df, 3)) {
    sub_df = base_df %>% filter(word %in% test_word)
    sub_sentence =  base_df %>% filter(line %in% unique(sub_df$line))
    counts = sub_df %>% count(word, sort = T)
    comb = bind_rows(comb, counts)
  }  
  
  result = comb %>% 
    group_by(word) %>% 
    summarise_all(sum) %>% 
    filter(word %in% options) %>% 
    arrange(desc(n))
  return(result)
}

files = c(blog, news, twitter)
all_lines = tibble()
for (file in files) {
  lines = tibble(text=read_lines(file))
  sample_lines = tibble(sample(lines$text, nrow(lines)*0.5))
  colnames(sample_lines) = 'text'
  all_lines = bind_rows(all_lines, sample_lines)
}

all_lines$text = gsub("[[:punct:]]|[[:digit:]]|(http[[:alpha:]]*:\\/\\/)","", all_lines$text)
all_lines$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", all_lines$text)
all_lines$text = gsub("@[^\\s]+", "", all_lines$text)
all_lines$text = stripWhitespace(all_lines$text)

text_df = all_lines %>%
  mutate(line=rownames(all_lines)) %>%
  unnest_tokens(word, text, token = "ngrams", n = 3) %>%
  anti_join(stop_words, by = "word")

unigram = readRDS("unigram_text.RDS")
bigram = readRDS("bigram_text.RDS")
trigram = readRDS("trigram_text.RDS")

head(bigram)

data = sample_n(bigram, nrow(bigram)*0.001)

first = c()
second = c()
for (item in strsplit(data$word, split=" ")) {
  first = c(first, item[1])
  second = c(second, item[2])
}

data1 = data.frame(first=first, second=second, line=data$line)

head(data1)

test_word = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
options = c("soda", "cheese", "pretzels", "beer")
findFreqWord(unigram, test_word, options, 1)
findFreqWord(bigram, test_word, options, 2)
findFreqWord(trigram, test_word, options, 3)


test_word = "You're the reason why I smile everyday. Can you follow me please? It would mean the"
options = c("best", "most", "universe", "world")
findFreqWord(test_word, options)

test_word = "Hey sunshine, can you follow me and make me the"
options = c("smelliest", "happiest", "saddist", "bluest")
findFreqWord(test_word, options)

# middle
test_word = "Very early observations on the Bills game: Offense still struggling but the"
options = c("referees", "crowd", "defense", "players")
findFreqWord(test_word, options)

# wrong
test_word = "Go on a romantic date at the"
options = c("beach", "grocery", "movies", "mall")
findFreqWord(test_word, options)

# wrong
test_word = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
options = c("horse", "way", "motocyle", "phone")


test_word = "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
options = c("thing", "weeks", "years", "time")

# wrong
test_word = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
options = c("fingers", "ears", "toes", "eyes")

# wrong
test_word = "Be grateful for the good times and keep the faith during the"
options = c("hard", "worse", "sad", "bad")

# wrong
test_word = "If this isn't the cutest thing you've ever seen, then you must be"
options = c("insane", "asleep", "insensitive", "callous")

findFreqWord(test_word, options)
