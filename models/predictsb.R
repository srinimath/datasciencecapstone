#stupid Backoff prediction implemetation
predictsb <- function(inputString,npred){
  is <- inputString
  is <- removePunctuation(is)
  is <- removeNumbers(is)
  is <- tolower(is)
  isn <- vapply(strsplit(is, "\\W+"), length, integer(1))
  isn
  probds <- data.frame(last=character(),
                       prob=character(),
                       stringsAsFactors=FALSE)
  if(isn==1){return(bigramfnsb(is,probds,npred))}
  if(isn==2){return(trigramfnsb(is,probds,npred))}
  if(isn>=3){
    is <- word(is,-3,-1)
    return(fourgramfnsb(is,probds,npred))
  }
}

bigramfnsb <- function(is,probds,npred) {
  bigram <- bigramf[bigramf$first==is,]
  if(nrow(probds)>0){
    bigram <- bigram[!(bigram$last %in% probds$last),]
  }
  probds <- head(probds,0)
  bigramcnt <- nrow(bigram)
  if(bigramcnt == 0){
    probds <- head(unigramf[order(-unigramf$probsb),c("ngram","probsb")],npred)
    return(probds)
  }
  probds <- unique(rbind(bigram[,c("last","probsb")],
                         setNames(probds,names(bigram[,c("last","probsb")]))))
  probds <- head(probds[order(-probds$probsb),],npred)
  return(probds)
}

trigramfnsb <- function(is,probds,npred) {
  trigram <- trigramf[trigramf$first==is,]
  if(nrow(probds)>0){
    trigram <- trigram[!(trigram$last %in% probds$last),]
  }
  trigramcnt <- nrow(trigram)
  if(trigramcnt == 0){
    bigramfirst <- word(is,-1)
    probds <- bigramfnsb(is,probds,npred)
    probds <- probds[!(probds$last %in% trigram$last),]
    probds$probsb <- 0.4*probds$probsb
    return(probds)
  }
  else if(trigramcnt<npred){
    bigramfirst <- word(is,-1)
    probds <- rbind(probds,trigram[,c("last","probsb")])
    print(probds)
    probds <- bigramfnsb(bigramfirst,probds,npred)
    probds$probsb <- 0.4*probds$probsb
  }
  probds <- unique(rbind(trigram[,c("last","probsb")],
                         setNames(probds,names(trigram[,c("last","probsb")]))))
  probds <- head(probds[order(-probds$probsb),],npred)
  return(probds)
}

fourgramfnsb <- function(is,probds,npred) {
  fourgram <- fourgramf[fourgramf$first==is,]
  fourgramcnt <- nrow(fourgram)
  if(fourgramcnt == 0){
    trigramfirst <- word(is,-2,-1)
    probds <- trigramfnsb(trigramfirst,probds,npred)
    probds$probsb <- 0.4*probds$probsb
    return(probds)
  }
  else if(fourgramcnt<npred){
    trigramfirst <- word(is,-2,-1)
    probds <- fourgram[,c("last","probsb")]
    probds <- trigramfnsb(trigramfirst,probds,npred)
    probds <- probds[!(probds$last %in% fourgram$last),]
    probds$probsb <- 0.4*probds$probsb
  }
  probds <- unique(rbind(fourgram[,c("last","probsb")],
                         setNames(probds,names(fourgram[,c("last","probsb")]))))
  probds <- head(probds[order(-probds$probsb),],npred)
  return(probds)
}
