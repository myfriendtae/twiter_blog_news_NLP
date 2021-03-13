library(stringr)
library(tm)
library(ggplot2)
library(ngram)

setwd('/Users/taepark/Dropbox/R/capstone project/data/')

path = 'en_US.blogs.txt'


tidyText(path, tidy_blog_file)
