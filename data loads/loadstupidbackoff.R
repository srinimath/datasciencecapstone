#UNIGRAM
load("./initial.RData")
allfreq <- sum(unigram$freq)
unigram$probsb <- unigram$freq/allfreq
unigram <- unigram[unigram$freq!=1,]
unigram <- unigram[!names(unigram) %in% c("freq")]
unigramsb <- unigram
rm(unigram)
save(unigramsb,file = "./unigramsb.RData")
gc()

#BIGRAM
bigram$ngram <- str_replace_all(bigram$ngram,"_"," ")
bigram$first <- word(bigram$ngram,1)
bigram$last <-word(bigram$ngram,-1)
bigram$firstfreq <- ave(bigram$freq,bigram$first,FUN=sum)
bigram$probsb <- bigram$freq/bigram$firstfreq
bigram <- bigram[bigram$freq != 1,]
bigram <- bigram[!names(bigram) %in% c("ngram","freq","firstfreq")]
bigramsb <- bigram
rm(bigram)
save(bigramsb,file = "./bigramsb.RData")
gc()

#TRIGRAM
trigram$ngram <- str_replace_all(trigram$ngram,"_"," ")
trigram$first <- word(trigram$ngram,1,2)
trigram$last <-word(trigram$ngram,-1)
trigram$firstfreq <- ave(trigram$freq,trigram$first,FUN=sum)
trigram$probsb <- trigram$freq/trigram$firstfreq
trigram <- trigram[trigram$freq != 1,]
trigram <- trigram[!names(trigram) %in% c("ngram","freq","firstfreq")]
trigramsb <- trigram
rm(trigram)
save(trigramsb,file = "./trigramsb.RData")
gc()

#FOURGRAM
fourgram$ngram <- str_replace_all(fourgram$ngram,"_"," ")
fourgram$first <- word(fourgram$ngram,1,3)
fourgram$last <-word(fourgram$ngram,-1)
fourgram$firstfreq <- ave(fourgram$freq,fourgram$first,FUN=sum)
fourgram$probsb <- fourgram$freq/fourgram$firstfreq
fourgram <- fourgram[fourgram$freq != 1,]
fourgram <- fourgram[!names(fourgram) %in% c("ngram","freq","firstfreq")]
fourgramsb <- fourgram
rm(fourgram)
save(fourgramsb,file = "./fourgramsb.RData")
gc()