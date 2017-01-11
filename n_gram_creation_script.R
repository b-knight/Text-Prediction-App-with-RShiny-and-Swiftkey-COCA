rm(list=ls())

# Read the data into R

setwd(paste("/Users/benjaminknight/Documents",
            "/Personal Training/Coursera",
            "/Data Science 10 - Capstone Project",
            "/Original Data/en_US", sep=""))
library(readr) 
lines <- readLines("en_US.news.txt")

# We will be working with R's n-gram package. 
# If the data object is too large, R will throw an error
# i.e., "Error: cannot allocate vector of size X"
# To avoid this problem I subset the data into 8 subsets.

delimiter <- round(length(lines)/10, 0)
subset_1 <- paste(rbind(lines[1:delimiter]), collapse = " ")
subset_2 <- paste(rbind(lines[(delimiter+1):(delimiter*2)]), collapse = " ")
subset_3 <- paste(rbind(lines[((delimiter*2)+1):(delimiter*3)]), collapse = " ")
subset_4 <- paste(rbind(lines[((delimiter*3)+1):(delimiter*4)]), collapse = " ")
subset_5 <- paste(rbind(lines[((delimiter*4)+1):(delimiter*5)]), collapse = " ")
subset_6 <- paste(rbind(lines[((delimiter*5)+1):(delimiter*6)]), collapse = " ")
subset_7 <- paste(rbind(lines[((delimiter*6)+1):(delimiter*7)]), collapse = " ")
subset_8 <- paste(rbind(lines[((delimiter*7)+1):(delimiter*8)]), collapse = " ")
subset_9 <- paste(rbind(lines[((delimiter*8)+1):(delimiter*9)]), collapse = " ")
subset_10 <- paste(rbind(lines[((delimiter*9)+1):length(lines)]), collapse = " ")

subsets <- list(subset_1, subset_2, subset_3, subset_4, subset_5, 
                subset_6, subset_7, subset_8, subset_9, subset_10)

rm(delimiter, lines, subset_1, subset_2, subset_3, subset_4, 
    subset_5, subset_6, subset_7, subset_8, subset_9, subset_10)

########################################################################################
# Preprocessing                                                                        #
########################################################################################

# We condition the subsets using gsub...

subsets <- lapply(subsets,  # strip out non-relevant characters
           function(x)      # prior to ngram construction
           {gsub(
           "\\w['-]\\w)|[[:punct:]]|_|,|\\*|;|\\=|\\^|\\+|\\]|\\[|>|<|\\}|
           ([[:alnum:]][-][[:alnum:]])|\\{|\\$|#|@|\\~|%|:|\\)|\\(|\"|\\|:|/", 
           '', x)})

# ...and then pre-condition them using ngram's preprocessing function

library(ngram) 
subsets <- lapply(subsets,          # preprocess the subsets for 
           function(x)              # conversion into ngrams
           {preprocess(x, 
           case = "lower", 
           remove.punct = FALSE,
           remove.numbers = TRUE, 
           fix.spacing = TRUE)})

########################################################################################
# Create unigrams                                                                      #
########################################################################################

raw_unigrams <- lapply(subsets,     # convert the subsets into unigrams
                function(x)
                {ngram(x, n = 1, sep = " ")})

library(data.table)
l = list(data.table(get.phrasetable(raw_unigrams[[1]])),
         data.table(get.phrasetable(raw_unigrams[[2]])),
         data.table(get.phrasetable(raw_unigrams[[3]])),
         data.table(get.phrasetable(raw_unigrams[[4]])),
         data.table(get.phrasetable(raw_unigrams[[5]])),
         data.table(get.phrasetable(raw_unigrams[[6]])),
         data.table(get.phrasetable(raw_unigrams[[7]])),
         data.table(get.phrasetable(raw_unigrams[[8]])),
         data.table(get.phrasetable(raw_unigrams[[9]])),
         data.table(get.phrasetable(raw_unigrams[[10]])))
unigrams <- rbindlist(l)
rm(l, raw_unigrams)

# Filter out non valid words using the English Wordlists by SIL International 
# http://www-01.sil.org/linguistics/wordlists/english/
valid_words <- data.table(
fread('http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt',
header = FALSE))
valid_words <- rename(valid_words, c("V1"="word"))

# aggregate subsets based on unigram
library(plyr)
unigrams <- subset(unigrams, select=-c(prop))
unigrams <- aggregate(unigrams$freq, by=list(unigrams$ngrams), FUN=sum)

# trim white space and extract valid unigrams
library(stringr)
unigrams$Group.1 <- str_trim(unigrams$Group.1) 
unigrams <- unigrams[unigrams$Group.1 %in% valid_words$word,]

# Re-tabulate frequencies and rename columns
unigrams$freq <- unigrams$x / sum(unigrams$x)
unigrams <- rename(unigrams, c("Group.1"="ngram", "x"="count"))

########################################################################################
# Create bigrams                                                                       #
########################################################################################
raw_bigrams <- lapply(subsets,          
               function(x)
               {ngram(x, n = 2, sep = " ")})

# The bigram parser function takes an ngram object and
# subsets it to only include bigrams the components of
# which are included in the English Wordlists by SIL International

bigram_parser <-  function(x, words) {
                  require(stringr)
                  x$A <- word(x$ngram, 1);
                  x$B <- word(x$ngram, 2);
                  x <- x[which(x$A %in% unlist(words) 
                             & x$B %in% unlist(words)), ]
                  x <- subset(x, select=-c(prop, A, B))
                  return(data.table(x))
}

bigram1  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[1]])), valid_words)
bigram2  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[2]])), valid_words)
bigram3  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[3]])), valid_words)
bigram4  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[4]])), valid_words)
bigram5  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[5]])), valid_words)
bigram6  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[6]])), valid_words)
bigram7  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[7]])), valid_words)
bigram8  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[8]])), valid_words)
bigram9  <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[9]])), valid_words)
bigram10 <- bigram_parser(data.table(
            get.phrasetable(raw_bigrams[[10]])), valid_words)

l = list(bigram1, bigram2, bigram3, bigram4, bigram5,
         bigram6, bigram7, bigram8, bigram9, bigram10)
bigrams <- rbindlist(l)
rm(l, bigram1, bigram2, bigram3, bigram4, bigram5,
   bigram6, bigram7, bigram8, bigram9, bigram10, raw_bigrams)

# Re-tabulate frequencies 

bigrams <- aggregate(bigrams$freq, by=list(bigrams$ngrams), FUN=sum)
bigrams$freq <- bigrams$x / sum(bigrams$x)
bigrams <- rename(bigrams, c("Group.1"="ngram", "x"="count"))

########################################################################################
# Create trigrams                                                                      #
########################################################################################

raw_trigrams <-    lapply(subsets,          
                   function(x)
                   {ngram(x, n = 3, sep = " ")})

trigram_parser <-  function(x, words) {
                   require(stringr)
                   x$A <- word(x$ngram, 1);
                   x$B <- word(x$ngram, 2);
                   x$C <- word(x$ngram, 3);
                   x <- x[which(x$A %in% unlist(words) 
                   & x$B %in% unlist(words)
                   & x$C %in% unlist(words)), ]
                   x <- subset(x, select=-c(prop, A, B, C))
                   return(data.table(x))
}

trigram1  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[1]])), valid_words)
trigram2  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[2]])), valid_words)
trigram3  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[3]])), valid_words)
trigram4  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[4]])), valid_words)
trigram5  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[5]])), valid_words)
trigram6  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[6]])), valid_words)
trigram7  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[7]])), valid_words)
trigram8  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[8]])), valid_words)
trigram9  <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[9]])), valid_words)
trigram10 <- trigram_parser(data.table(
             get.phrasetable(raw_trigrams[[10]])), valid_words)

l = list(trigram1, trigram2, trigram3, trigram4, trigram5,
         trigram6, trigram7, trigram8, trigram9, trigram10)
trigrams <- rbindlist(l)
rm(l, trigram1, trigram2, trigram3, trigram4, trigram5,
   trigram6, trigram7, trigram8, trigram9, trigram10, raw_trigrams)

# Re-tabulate frequencies 

trigrams <- aggregate(trigrams$freq, by=list(trigrams$ngrams), FUN=sum)
trigrams$freq <- trigrams$x / sum(trigrams$x)
trigrams <- rename(trigrams, c("Group.1"="ngram", "x"="count"))

########################################################################################
# Store the results in the local machine                                               #
########################################################################################

setwd(paste("/Users/benjaminknight/Documents",
            "/Personal Training/Coursera",
            "/Data Science 10 - Capstone Project", sep=""))
if (!file.exists("ngrams")) {dir.create("ngrams")}
setwd("./ngrams")
fwrite(unigrams, "unigrams.csv")
fwrite(bigrams, "bigrams.csv")
fwrite(trigrams, "trigrams.csv")
