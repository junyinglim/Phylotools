#newick and taxo2newick
#newick returns a list of taxa names into a polytomic newick tree
#taxo2newick returns a dataframe of taxa names into a newick tree,
#assuming monophyly in the penultimate taxonomic hierarchy (e.g., if data
#is on genera, then confamilial genera will be a monophyletic polytomy)

newick <- function(x){
  base <- x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      base <- paste(base, x[i], sep = ", ")
    }
    return(paste("(", base, ")", sep = ""))
  } else {
    return(base)
  }
}

taxo2newick <-function(x, backbone = TRUE, backbonenewick, hightaxo, lowtaxo){
  taxo <- c("order", "class", "family")
  if(sum(match(tolower(names(x)), taxo, nomatch=0)) == 0){
    stop("Dataframe does not have taxonomic classifications")
  }else{
    hightaxoindex <- match(hightaxo, names(x))   #Find index of desired taxa
    hightaxolist <- as.vector(unique(x[, hightaxoindex]))   #Figure out what are the levels in the particular
    if(backbone == TRUE){
      base <- backbonenewick
    } else {
      base <- newick(hightaxolist)
    }
    for(i in 1:length(hightaxolist)){
      temp <- subset(x, x[,hightaxoindex] == hightaxolist[i]) #Subsets dataframe based on higher taxonomy to find the lower taxonomic levels
      lowtaxolist <- as.vector(unique(temp[,match(lowtaxo, names(temp))]))
      temp2 <- newick(lowtaxolist)
      base <- gsub(hightaxolist[i], temp2, base)
      print(base)
    }
  }
  return(base)
}

#TESTING
taxo <- data.frame(Class = c("A", "A", "B", "B", "C"), Order = c("D", "E", "F", "G", "H"), Family = c("I","J", "K", "L", "M"))
classorder <- taxo2newick(taxo, backbone = FALSE, hightaxo = "Class", lowtaxo = "Order")
orderfamily <- taxo2newick(taxo, backbone = TRUE, hightaxo = "Order", lowtaxo = "Family", backbonenewick = classorder)
