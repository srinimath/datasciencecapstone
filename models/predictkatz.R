#katz backoff method
predictkatz <- function(inputString,npred){
  is <- inputString
  is <- removePunctuation(is)
  #is <- removeNumbers(is)
  is <- tolower(is)
  isn <- vapply(strsplit(is, "\\W+"), length, integer(1))
  probds <- data.frame(last=character(),
                   prob=character(),
                   stringsAsFactors=FALSE)
  if(isn==1){return(bigramfnkatz(is,probds,npred))}
  if(isn==2){return(trigramfnkatz(is,probds,npred))}
  if(isn>=3){
    is <- word(is,-3,-1)
  return(fourgramfnkatz(is,probds,npred))
    }
}


bigramfnkatz <- function(is,probds,npred){
  bigram <- bigramf[bigramf$first==is,]
  bigramcnt <- nrow(bigram)
  if(bigramcnt == 0){
    probds <- head(unigramf[order(-unigramf$probkatz), c("ngram","probkatz")],npred)
    return(probds)
  }
  else if(bigramcnt < npred){
    leftoverprob <- unique(bigramf$leftoverprob[bigramf$first==is])
    bigramlast <- bigramf$last[bigramf$first==is]
    probds <- head(unigramf[order(-unigramf$probkatz), c("ngram","freq","discountcoeff")],10)
    probds <- probds[!(probds$ngram %in% bigramlast),]
    denom <- sum(probds$freq*probds$discountcoeff)
    probds$probkatz <- (probds$freq*probds$discountcoeff)*leftoverprob/denom
    probds <- probds[!(names(probds) %in% c("discountcoeff","freq"))]
  }
  temp <- bigramf[bigramf$first==is,c("last","probkatz")]
  temp <- head(temp[order(-temp$probkatz),],npred)
  probds <- unique(rbind(temp,setNames(probds,names(temp))))
  probds <- head(probds[order(-probds$probkatz),],npred)
  return(probds)
}

trigramfnkatz <- function(is,probds,npred){
  trigram <- trigramf[trigramf$first==is,]
  trigramcnt <- nrow(trigram)
  if(trigramcnt==0){is <- word(is,-1); return(bigramfnkatz(is,probds,npred))}
  else if(trigramcnt<npred){
    leftoverprob <- unique(trigramf$leftoverprob[trigramf$first==is])
    bigramfirst <- word(is,-1)
    trigramlast <- trigramf$last[trigramf$first==is]
    probdsbi <- bigramf[bigramf$first==bigramfirst,c("last","freq","discountcoeff")]
    bigramcnt <- nrow(probds)
    if(bigramcnt != 0){
      probdsbi <- probdsbi[!(probdsbi$last %in% trigramlast),]
      denom <- sum(probdsbi$freq*probdsbi$discountcoeff)
      probdsbi$probkatz <- (probdsbi$freq*probdsbi$discountcoeff)*leftoverprob/denom
      probdsbi <- probdsbi[!(names(probdsbi) %in% c("discountcoeff","freq"))]
      probds <-probds <- rbind(probds,setNames(probdsbi,names(probds)))
    }
    if(trigramcnt+bigramcnt < npred){
      probdsuni <- head(unigramf[order(-unigramf$probkatz), c("ngram","freq","discountcoeff")],10)
      probdsuni <- probdsuni[!(probdsuni$ngram %in% trigramlast),]
      denom <- sum(probdsuni$freq*probdsuni$discountcoeff)
      probdsuni$probkatz <- (probdsuni$freq*probdsuni$discountcoeff)*leftoverprob/denom
      probdsuni <- probdsuni[!(names(probdsuni) %in% c("discountcoeff","freq"))]
      probds <- rbind(probds,setNames(probdsuni,names(probds)))
    }
  }
  temp <- trigramf[trigramf$first==is,c("last","probkatz")]
  temp <- head(temp[order(-temp$probkatz),],npred)
  probds <- unique(rbind(temp,setNames(probds,names(temp))))
  probds <- head(probds[order(-probds$probkatz),],npred)
  return(probds)
}

fourgramfnkatz <- function(is,probds,npred){
  fourgram <- fourgramf[fourgramf$first==is,]
  fourgramcnt <- nrow(fourgram)
  if(fourgramcnt==0){is <- word(is,-2,-1); return(trigramfnkatz(is,probds,npred))}
  else if(fourgramcnt<npred){
    leftoverprob <- unique(fourgramf$leftoverprob[fourgramf$first==is])
    trigramfirst <- word(is,-1)
    fourgramlast <- fourgramf$last[fourgramf$first==is]
    probdstri <- trigramf[trigramf$first==trigramfirst,c("last","freq","discountcoeff")]
    trigramcnt <- nrow(probds)
    if(trigramcnt != 0){
      probdstri <- probdstri[!(probdstri$last %in% fourgramlast),]
      denom <- sum(probdstri$freq*probdstri$discountcoeff)
      probdstri$probkatz <- (probdstri$freq*probdstri$discountcoeff)*leftoverprob/denom
      probdstri <- probdstri[!(names(probdstri) %in% c("discountcoeff","freq"))]
      probds <- rbind(probds,setNames(probdstri,names(probds)))
    }
    if(fourgramcnt+trigramcnt < npred){
      bigramfirst <- word(trigramfirst,-1)
      probdsbi <- bigramf[bigramf$first==bigramfirst,c("last","freq","discountcoeff")]
      bigramcnt <- nrow(probdsbi)
      if(bigramcnt != 0){
        probdsbi <- probdsbi[!(probdsbi$last %in% fourgramlast),]
        denom <- sum(probdsbi$freq*probdsbi$discountcoeff)
        probdsbi$probkatz <- (probdsbi$freq*probdsbi$discountcoeff)*leftoverprob/denom
        probdsbi <- probdsbi[!(names(probdsbi) %in% c("discountcoeff","freq"))]
        probds <- rbind(probds,setNames(probdsbi,names(probds)))
      }
      if(fourgramcnt+trigramcnt+bigramcnt < npred){
        probdsuni <- head(unigramf[order(-unigramf$probkatz), c("ngram","freq","discountcoeff")],10)
        probdsuni <- probdsuni[!(probdsuni$ngram %in% fourgramlast),]
        denom <- sum(probdsuni$freq*probdsuni$discountcoeff)
        probdsuni$probkatz <- (probdsuni$freq*probdsuni$discountcoeff)*leftoverprob/denom
        probdsuni <- probdsuni[!(names(probdsuni) %in% c("discountcoeff","freq"))]
        probds <- rbind(probds,setNames(probdsuni,names(probds)))
      }
    }
  }
  temp <- fourgramf[fourgramf$first==is,c("last","probkatz")]
  temp <- head(temp[order(-temp$probkatz),],npred)
  probds <- unique(rbind(temp,setNames(probds,names(temp))))
  probds <- head(probds[order(-probds$probkatz),],npred)
  return(probds)
}



