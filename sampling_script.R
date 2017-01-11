rm(list=ls())
library(readr)
setwd(paste("/Users/benjaminknight/Documents",
            "/Personal Training/Coursera",
            "/Data Science 10 - Capstone Project",
            "/final/en_US", sep=""))


twitter_data <- read_file("en_US.twitter.txt")
end <- nchar(twitter_data)/1000 
twitter_sample <- substr(twitter_data, 0, end)
fileConn <-file("twitter_sample.txt")
writeLines(twitter_sample, fileConn)
close(fileConn)

blog_data <- read_file("en_US.blogs.txt")
end_2 <- nchar(blog_data)/1000 
blog_sample <- substr(blog_data, 0, end_2)
fileConn_2 <-file("blog_sample.txt")
writeLines(blog_sample, fileConn_2)
close(fileConn_2)

news_data <- read_file("en_US.news.txt")
end_3 <- nchar(news_data)/1000 
news_sample <- substr(news_data, 0, end_3)
fileConn_3 <-file("news_sample.txt")
writeLines(news_sample, fileConn_3)
close(fileConn_3)


