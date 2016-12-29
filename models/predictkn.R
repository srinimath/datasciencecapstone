#kneser-ney interpolation method
predictkn <- function(inputString,npred){
  is <- inputString
  is <- removePunctuation(is)
  is <- removeNumbers(is)
  is <- tolower(is)
  isn <- vapply(strsplit(is, "\\W+"), length, integer(1))
  isn
  probds <- data.frame(last=character(),
                       prob=character(),
                       stringsAsFactors=FALSE)
  if(isn==1){return(bigramfnkn(is,probds,npred))}
  if(isn==2){return(trigramfnkn(is,probds,npred))}
  if(isn>=3){
    is <- word(is,-3,-1)
    return(fourgramfnkn(is,probds,npred))
  }
}

bigramfnkn <- function(is,probds,npred) {
  bigram <- bigramf[bigramf$first==is,]
  if(nrow(probds)>0){
    bigram <- bigram[!(bigram$last %in% probds$last),]
  }
  bigramcnt <- nrow(bigram)
  if(bigramcnt == 0){
    return(probds)
  }
  probds <- unique(rbind(bigram[,c("last","pkn")],
                         setNames(probds,names(bigram[,c("last","pkn")]))))
  probds <- head(probds[order(-probds$pkn),],npred)
  return(probds)
}

trigramfnkn <- function(is,probds,npred) {
  trigram <- trigramf[trigramf$first==is,]
  if(nrow(probds)>0){
    trigram <- trigram[!(trigram$last %in% probds$last),]
  }
  trigramcnt <- nrow(trigram)
  if(trigramcnt == 0){
    bigramfirst <- word(is,-1)
    return(bigramfnkn(is,probds,npred))
  }
  else if(trigramcnt<npred){
    bigramfirst <- word(is,-1)
    probds <- rbind(probds,trigram[,c("last","pkn")])
    probds <- bigramfnkn(bigramfirst,probds,npred)
  }
  probds <- unique(rbind(trigram[,c("last","pkn")],
                         setNames(probds,names(trigram[,c("last","pkn")]))))
  probds <- head(probds[order(-probds$pkn),],npred)
  return(probds)
}

fourgramfnkn <- function(is,probds,npred) {
  fourgram <- fourgramf[fourgramf$first==is,]
  fourgramcnt <- nrow(fourgram)
  if(fourgramcnt == 0){
    trigramfirst <- word(is,-2,-1)
    return(trigramfnkn(trigramfirst,probds,npred))
  }
  else if(fourgramcnt<npred){
    trigramfirst <- word(is,-2,-1)
    probds <- fourgram[,c("last","pkn")]
    probds <- trigramfnkn(trigramfirst,probds,npred)
  }
  probds <- unique(rbind(fourgram[,c("last","pkn")],
                         setNames(probds,names(fourgram[,c("last","pkn")]))))
  probds <- head(probds[order(-probds$pkn),],npred)
  return(probds)
}
