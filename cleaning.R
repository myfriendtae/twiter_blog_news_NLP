# Capstone project
library(readtext)
library(R.utils)

setwd('/Users/taepark/Downloads/final/en_US')

# checking the number lines
NROW(readLines('en_US.twitter.txt'))

con = file('en_US.twitter.txt')
con = file('en_US.news.txt')
con = file('en_US.blogs.txt')

# checking the length of each line.
con = file('en_US.twitter.txt')
lenn = nchar(readLines(con))
max(lenn)
close(con)

# checking the number of lines with the words: 'love' and 'hate'
con = file('en_US.twitter.txt')
lovec = grepl(".love.", readLines(con), ignore.case = F)
hatec = grepl(".hate.", readLines(con), ignore.case = F)
close(con)
sum(lovec)/sum(hatec)

# find the line with the specific word, 'biostats'
con = file('en_US.twitter.txt')
biostatsLine = grepl(".biostats.", readLines(con), ignore.case = F)
readLines(con)[biostatsLine]
close(con)

# find the number of tweets with exact characters as the following.
exactline = grepl('A computer once beat me at chess, but it was no match for me at kickboxing', readLines(con))
sum(exactline)
close(con)


