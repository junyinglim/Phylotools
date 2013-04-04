#newick and taxo2newick

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

taxo2newick <-function(x, backbone = TRUE, backbonenewick){
  taxo <- c("order", "class", "family")
  if(sum(match(tolower(names(x)), taxo, nomatch=0)) == 0){
    stop("Dataframe does not have taxonomic classifications")
  }else{
    if(backbone == TRUE){
      base <- backbonenewick
    } else {
    base <- newick(unique(x$Class))
    }
    for(i in 1:length(unique(x$Class))){
      temp <- subset(x, Class == unique(x$Class)[i])
      temp2 <- newick(as.vector(unique(temp$Order)))
      base <- gsub(unique(x$Class)[i], temp2, base)
      for(j in 1:length(unique(temp$Order))){
        lowertemp <- subset(temp, Order == unique(temp$Order)[j])
        lowertemp2 <- newick(as.vector(unique(lowertemp$Family)))
        base <- gsub(unique(temp$Order)[j], lowertemp2, base)
      }
    }
  }
  return(paste(base, ";", ""))
}

#TESTING
taxo <- data.frame(Class = c("A", "A", "B", "B", "C"), Order = c("D", "E", "F", "G", "H"), Family = c("I","J", "K", "L", "M"))
noconst <- taxo2newick(taxo)
const <- taxo2newick(taxo, backbone = TRUE, backbonenewick = "((A, C), B)")