gc()
load("./initial.RData")
calcDiscount <- function(n1,n2){
discount <- n1+2*n2
discount <- n1/discount
return(discount)
}

#######BIGRAMS#########
#load bigrams
#you doing
#you
bigramkn <- bigram
rm(bigram)
bigramkn$ngram <- str_replace_all(bigramkn$ngram,"_"," ")
bigramkn$first <- word(bigramkn$ngram,1)
bigramkn$last <-word(bigramkn$ngram,-1)
bigramkn$prelastfreq <- ave(bigramkn$freq,bigramkn$last,FUN=sum)
bigramkn$prefirstfreq <- bigramkn$prelastfreq[match(bigramkn$first,bigramkn$last)]
bigramkn$sucfirstfreq <- ave(bigramkn$freq,bigramkn$first,FUN=sum)
n <- sum(bigramkn$freq)

#calculate discount..use absolute discounting
n1 <- nrow(bigramkn[bigramkn$freq==1,])
n2 <- nrow(bigramkn[bigramkn$freq==2,])
discount <- calcDiscount(n1,n2)

trigramkn <- trigram
rm(trigram)
trigramkn$ngram <- str_replace_all(trigramkn$ngram,"_"," ")
trigramkn$last2 <- word(trigramkn$ngram,-2,-1)
trigramkn$last2freq <- ave(trigramkn$freq,trigramkn$last2,FUN=sum)
bigramkn$preall <- trigramkn$last2freq[match(bigramkn$ngram,trigramkn$last2)]

#calculate continuation probability for 2-gram
bigramkn$pkn <- ((bigramkn$preall-discount)/bigramkn$prefirstfreq)+
  (((discount/bigramkn$prefirstfreq)*bigramkn$sucfirstfreq)*(bigramkn$prelastfreq/n))
bigramkn$pkn[is.na(bigramkn$pkn)] <- 0
bigramkn <- bigramkn[!names(bigramkn) %in% c("prelastfreq","prefirstfreq","preall",
                                             "sucfirstfreq") ]
save(bigramkn,file = "bigramkn.RData")
gc()


#######TRIGRAMS#########
trigramkn$prevpkn <- bigramkn$pkn[match(trigramkn$last2,bigramkn$ngram)]
rm(bigramkn)
gc()

if(nrow(trigramkn[is.na(trigramkn$prevpkn),]) > 0){
  trigramkn$prevpkn[is.na(trigramkn$prevpkn)] <- 0
}
trigramkn$first <- word(trigramkn$ngram,1,2)
trigramkn$last <-word(trigramkn$ngram,-1)
trigramkn$prefirstfreq  <- trigramkn$last2freq[match(trigramkn$first,trigramkn$last2)]
trigramkn$sucfirstfreq <- ave(trigramkn$freq,trigramkn$first,FUN=sum)
trigramkn <- trigramkn[!names(trigramkn) %in% c("last2","last2freq")]
gc()
n <- sum(trigramkn$freq)

#calculate discount..use absolute discounting
n1 <- nrow(trigramkn[trigramkn$freq==1,])
n2 <- nrow(trigramkn[trigramkn$freq==2,])
discount <- calcDiscount(n1,n2)

fourgramkn <- fourgram
rm(fourgram)
fourgramkn$ngram <- str_replace_all(fourgramkn$ngram,"_"," ")
fourgramkn$last3 <- word(fourgramkn$ngram,-3,-1)
fourgramkn$last3freq <- ave(fourgramkn$freq,fourgramkn$last3,FUN=sum)
trigramkn$preall <- fourgramkn$last3freq[match(trigramkn$ngram,fourgramkn$last3)]

#calculate continuation probability for 3-gram
trigramkn$pkn <- ((trigramkn$preall-discount)/trigramkn$prefirstfreq)+
  (((discount/trigramkn$prefirstfreq)*trigramkn$sucfirstfreq)*trigramkn$prevpkn)
if(nrow(trigramkn[is.na(trigramkn$pkn),])>0){
  trigramkn$pkn[is.na(trigramkn$pkn)] <- 0
} 
trigramkn <- trigramkn[!names(trigramkn) %in% c("prefirstfreq","preall",
                                             "sucfirstfreq","prevpkn") ]
save(trigramkn,file = "trigramkn.RData")
gc()



#######FOURGRAMS#########
fourgramkn$prevpkn <- trigramkn$pkn[match(fourgramkn$last3,trigramkn$ngram)]
gc()

if(nrow(fourgramkn[is.na(fourgramkn$prevpkn),]) > 0){
  fourgramkn$prevpkn[is.na(fourgramkn$prevpkn)] <- 0
}
fourgramkn$first <- word(fourgramkn$ngram,1,3)
fourgramkn$last <-word(fourgramkn$ngram,-1)
fourgramkn$sucfirstfreq <- ave(fourgramkn$freq,fourgramkn$first,FUN=sum)
fourgramkn$n1gramfreq <- trigramkn$freq[match(fourgramkn$first,trigramkn$ngram)]
fourgramkn <- fourgramkn[!names(fourgramkn) %in% c("last3","last3freq")]
rm(trigramkn)
gc()
n <- sum(fourgramkn$freq)

#calculate discount..use absolute discounting
n1 <- nrow(fourgramkn[fourgramkn$freq==1,])
n2 <- nrow(fourgramkn[fourgramkn$freq==2,])
discount <- calcDiscount(n1,n2)

fourgramkn$ngramfreq <- ave(fourgramkn$freq,fourgramkn$ngram,FUN=sum)


#calculate continuation probability for 3-gram
fourgramkn$pkn <- ((fourgramkn$ngramfreq-discount)/fourgramkn$n1gramfreq)+
  (((discount/fourgramkn$n1gramfreq)*fourgramkn$sucfirstfreq)*fourgramkn$prevpkn)
if(nrow(fourgramkn[is.na(fourgramkn$pkn),])>0){
  fourgramkn$pkn[is.na(fourgramkn$pkn)] <- 0
} 

fourgramkn <- fourgramkn[!names(fourgramkn) %in% c("ngram","prevpkn",
                                                "sucfirstfreq","ngramfreq","n1gramfreq") ]
save(fourgramkn,file = "fourgramkn.RData")
gc()

load("./trigramkn.RData")
#trigramkn <- trigramkn[!names(trigramkn) %in% c("ngram","freq")]
gc()

load("./bigramkn.RData")
#bigramkn <- bigramkn[!names(bigramkn) %in% c("ngram")]
gc()

fourgramkn <- fourgramkn[fourgramkn$freq != 1,]
trigramkn <- trigramkn[trigramkn$freq != 1,]
bigramkn <- bigramkn[bigramkn$freq != 1,]

fourgramkn <- fourgramkn[!names(fourgramkn) %in% c("ngram","freq")]
trigramkn <- trigramkn[!names(trigramkn) %in% c("ngram","freq")]
bigramkn <- bigramkn[!names(bigramkn) %in% c("ngram","freq")]




