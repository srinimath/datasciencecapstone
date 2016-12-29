unigramf <- merge(unigramkatz,unigramsb,by = c("ngram"))
rm(unigramsb); rm(unigramkatz)
gc()

bigramf <- merge(merge(bigramkatz,bigramkn,by = c("first","last")),
                 bigramsb,by = c("first","last"))
rm(bigramkatz);rm(bigramkn);rm(bigramsb)
gC()

trigramf <- merge(merge(trigramkatz,trigramkn,by = c("first","last")),
      trigramsb,by = c("first","last"))
rm(trigramkatz);rm(trigramkn);rm(trigramsb)
gc()

fourgramf <- merge(merge(fourgramkatz,fourgramkn,by = c("first","last")),
                  fourgramsb,by = c("first","last"))
rm(fourgramkatz);rm(fourgramkn);rm(fourgramsb)
gc()

save(unigramf,bigramf,trigramf,fourgramf,file = "textprediction.RData")