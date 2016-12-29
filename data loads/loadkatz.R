#fourgram
load("./initial.RData")
fourgramkatz <- fourgram
rm(fourgram)

#rearrange data
fourgramkatz <- fourgramkatz[fourgramkatz$freq != 1,]
fourgramkatz$first <- gsub("_"," ",word(fourgramkatz$ngram,1,3,sep = "_"))
fourgramkatz$last <- word(fourgramkatz$ngram,-1,sep = "_")
fourgramkatz <- fourgramkatz[names(fourgramkatz) != "ngram"]


#calculate discount coefficient
fourgramkatz$discountcoeff <- 1
for(i in 1:5){
  icount <- nrow(fourgramkatz[fourgramkatz$freq==i,])
  inextcount <- nrow(fourgramkatz[fourgramkatz$freq==i+1,])
  idiscountcoeff <- ((i+1)*(inextcount))/(i*icount)
  fourgramkatz$discountcoeff[fourgramkatz$freq==i] <- idiscountcoeff
}

#calculateprobability and left over
fourgramkatz$leftoverprob <- 0
fourgramkatz$probkatz <- 0
k <- " "
for(i in fourgramkatz$first){
  if(i==k) next
  temp <- fourgramkatz[fourgramkatz$first==i,]
  totalfreq <- sum(temp$freq[temp$first==i])
  totaldiscfreq <- sum(temp$freq[temp$first==i]*temp$discountcoeff[temp$first==i])
  leftoverprob <- 1-(totaldiscfreq/totalfreq)
  for(j in temp$last){
    freq <- fourgramkatz$freq[fourgramkatz$first==i & fourgramkatz$last==j]
    fourgramkatz$probkatz[fourgramkatz$first==i & fourgramkatz$last==j] <- freq/totalfreq
  }
  fourgramkatz$leftoverprob[fourgramkatz$first==i] <- leftoverprob
  k <- i
  rm(temp)
}
save(fourgramkatz,file = "fourgramkatz.RData")

#trigram
trigramkatz <- trigram
rm(trigram)

#rearrange data
trigramkatz <- trigramkatz[trigramkatz$freq != 1,]
trigramkatz$first <- gsub("_"," ",word(trigramkatz$ngram,1,2,sep = "_"))
trigramkatz$last <- word(trigramkatz$ngram,-1,sep = "_")
trigramkatz <- trigramkatz[names(trigramkatz) != "ngram"]


#calculate discount coefficient
trigramkatz$discountcoeff <- 1
for(i in 1:5){
  icount <- nrow(trigramkatz[trigramkatz$freq==i,])
  inextcount <- nrow(trigramkatz[trigramkatz$freq==i+1,])
  idiscountcoeff <- ((i+1)*(inextcount))/(i*icount)
  trigramkatz$discountcoeff[trigramkatz$freq==i] <- idiscountcoeff
}

#calculateprobability and left over
trigramkatz$leftoverprob <- 0
trigramkatz$probkatz <- 0
k <- " "
for(i in trigramkatz$first){
  if(i==k) next
  temp <- trigramkatz[trigramkatz$first==i,]
  totalfreq <- sum(temp$freq[temp$first==i])
  totaldiscfreq <- sum(temp$freq[temp$first==i]*temp$discountcoeff[temp$first==i])
  leftoverprob <- 1-(totaldiscfreq/totalfreq)
  for(j in temp$last){
    freq <- trigramkatz$freq[trigramkatz$first==i & trigramkatz$last==j]
    trigramkatz$probkatz[trigramkatz$first==i & trigramkatz$last==j] <- freq/totalfreq
  }
  trigramkatz$leftoverprob[trigramkatz$first==i] <- leftoverprob
  k <- i
  rm(temp)
}
save(trigramkatz,file = "trigramkatz.RData")


#bigram
bigramkatz <- bigram
rm(bigram)

#rearrange data
bigramkatz <- bigramkatz[bigramkatz$freq != 1,]
bigramkatz$first <- gsub("_"," ",word(bigramkatz$ngram,1,1,sep = "_"))
bigramkatz$last <- word(bigramkatz$ngram,-1,sep = "_")
bigramkatz <- bigramkatz[names(bigramkatz) != "ngram"]


#calculate discount coefficient
bigramkatz$discountcoeff <- 1
for(i in 1:5){
  icount <- nrow(bigramkatz[bigramkatz$freq==i,])
  inextcount <- nrow(bigramkatz[bigramkatz$freq==i+1,])
  idiscountcoeff <- ((i+1)*(inextcount))/(i*icount)
  bigramkatz$discountcoeff[bigramkatz$freq==i] <- idiscountcoeff
}

#calculateprobability and left over
bigramkatz$leftoverprob <- 0
bigramkatz$probkatz <- 0
k <- " "
for(i in sort(bigramkatz$first)){
  if(i==k) {next}
  temp <- bigramkatz[bigramkatz$first==i,]
  totalfreq <- sum(temp$freq[temp$first==i])
  totaldiscfreq <- sum(temp$freq[temp$first==i]*temp$discountcoeff[temp$first==i])
  leftoverprob <- 1-(totaldiscfreq/totalfreq)
  for(j in temp$last){
    freq <- bigramkatz$freq[bigramkatz$first==i & bigramkatz$last==j]
    bigramkatz$probkatz[bigramkatz$first==i & bigramkatz$last==j] <- freq/totalfreq
  }
  bigramkatz$leftoverprob[bigramkatz$first==i] <- leftoverprob
  k <- i
  rm(temp)
}
save(bigramkatz,file = "bigramkatz.RData")

#unigram
unigramkatz <- unigram
rm(bigram)

#rearrange data
unigramkatz <- unigramkatz[unigramkatz$freq != 1,]
totunigramfreq <- sum(unigramkatz$freq)
unigramkatz$probkatz <- unigramkatz$freq/totunigramfreq

#calculate discount coefficient
unigramkatz$discountcoeff <- 1
for(i in 1:5){
  icount <- nrow(unigramkatz[unigramkatz$freq==i,])
  inextcount <- nrow(unigramkatz[unigramkatz$freq==i+1,])
  idiscountcoeff <- ((i+1)*(inextcount))/(i*icount)
  unigramkatz$discountcoeff[unigramkatz$freq==i] <- idiscountcoeff
}

save(unigramkatz,file = "unigramkatz.RData")

