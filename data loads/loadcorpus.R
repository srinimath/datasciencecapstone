setwd("C:/Personal/Srini/Courses/Data Science/10 - CapStone Project")
library(stringi)
library(stringr)
library(ggplot2)
library(RWeka)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(gridExtra)
library(dplyr)
library(data.table)
library(markovchain)
library(ngram)
library(quanteda)
library(tau)

#download and unzip data zip file
#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")
#unzip("Coursera-SwiftKey.zip")

filesinfo <- file.info(list.files("final/en_US/",all.files = TRUE,
                                  recursive = TRUE, full.names = TRUE))
filecnt <- 0
while (filecnt < nrow(filesinfo)){

  readlines <- 100000
  linesread <- readlines
  nooflines <- 0
  testcon <- file(rownames(filesinfo)[filecnt+1],open="r")
  while(linesread> 0){
    data <- readLines(testcon,readlines)
    linesread <- length(data)
    if(linesread==0){break}
    data <- iconv(data, "latin1", "ASCII", sub="")
    data <- iconv(data, "latin1", "UTF-8", sub="")
    data <- str_replace_all(data, "[^[:alnum:]\\s]", "")
    myCorpus <- corpus(data)
    rm(data)

    #unigram
    temp <- dfm(myCorpus, verbose = TRUE, removePunct = TRUE, removeNumbers = TRUE)
    temp <-  as.data.frame(colSums(temp))
    temp <- transmute(temp, ngram=rownames(temp),freq=`colSums(temp)`)

    if(nooflines==0) {unigram <- temp}
    else {
      inter <- merge(unigram,temp,by.x = "ngram",by.y = "ngram", all.y = TRUE)
      unigram <- inter[!is.na(inter$freq.x),]
      temp <- inter[is.na(inter$freq.x),]
      rm(inter)
      unigram <- transmute(unigram, ngram=ngram, freq=`freq.x`+`freq.y`)
      temp <- transmute(temp, ngram=ngram, freq=`freq.y`)
      unigram <- rbind(unigram,temp)
    }
    gc()

    #bigram
    temp <- dfm(myCorpus, ngram = 2, verbose = TRUE, removePunct = TRUE, removeNumbers = TRUE)
    temp <-  as.data.frame(colSums(temp))
    temp <- transmute(temp, ngram=rownames(temp),freq=`colSums(temp)`)

    if(nooflines==0) {bigram <- temp}
    else {
      inter <- merge(bigram,temp,by.x = "ngram",by.y = "ngram", all.y = TRUE)
      bigram <- inter[!is.na(inter$freq.x),]
      temp <- inter[is.na(inter$freq.x),]
      rm(inter)
      bigram <- transmute(bigram, ngram=ngram, freq=`freq.x`+`freq.y`)
      temp <- transmute(temp, ngram=ngram, freq=`freq.y`)
      bigram <- rbind(bigram,temp)
    }
    gc()

    #trigram
    temp <- dfm(myCorpus, ngram = 3, verbose = TRUE, removePunct = TRUE, removeNumbers = TRUE)
    temp <-  as.data.frame(colSums(temp))
    temp <- transmute(temp, ngram=rownames(temp),freq=`colSums(temp)`)

    if(nooflines==0) {trigram <- temp}
    else {
      inter <- merge(trigram,temp,by.x = "ngram",by.y = "ngram", all.y = TRUE)
      trigram <- inter[!is.na(inter$freq.x),]
      temp <- inter[is.na(inter$freq.x),]
      rm(inter)
      trigram <- transmute(trigram, ngram=ngram, freq=`freq.x`+`freq.y`)
      temp <- transmute(temp, ngram=ngram, freq=`freq.y`)
      trigram <- rbind(trigram,temp)
    }
    gc()

    #fourgram
    temp <- dfm(myCorpus, ngram = 4, verbose = TRUE, removePunct = TRUE, removeNumbers = TRUE)
    temp <-  as.data.frame(colSums(temp))
    temp <- transmute(temp, ngram=rownames(temp),freq=`colSums(temp)`)


    if(nooflines==0) {fourgram <- temp}
    else {
      inter <- merge(fourgram,temp,by.x = "ngram",by.y = "ngram", all.y = TRUE)
      fourgram <- inter[!is.na(inter$freq.x),]
      temp <- inter[is.na(inter$freq.x),]
      rm(inter)
      fourgram <- transmute(fourgram, ngram=ngram, freq=`freq.x`+`freq.y`)
      temp <- transmute(temp, ngram=ngram, freq=`freq.y`)
      fourgram <- rbind(fourgram,temp)
    }

    nooflines <- nooflines+linesread
    rm(myCorpus)
    rm(temp)
    gc()
  }
  close(testcon)
  filecnt <- 1+filecnt
}
# unigram <- unigram[unigram$freq>=5,]
# bigram <- bigram[bigram$freq>=5,]
# trigram <- trigram[trigram$freq>=5,]
# fourgram <- fourgram[fourgram$freq>=5,]

save(unigram,bigram,trigram,fourgram,file = "./initial.RData")
rm(unigram);rm(bigram);rm(trigram);rm(fourgram);
gc()

